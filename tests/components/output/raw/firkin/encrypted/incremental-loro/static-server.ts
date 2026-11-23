// @ts-nocheck -- test server, intentionally untyped

/**
 * Serve the built site with a script injected before the app's modules.
 *
 * The test browser has no WebCrypto Ed25519, and the real app signs session
 * tokens, manifests and hints with it. The other tests install an Ed25519 shim
 * from the page after it loads, which is gone on a reload. Injecting the shim
 * (and the request counters) into every HTML response is what makes a **real**
 * reload testable: the app boots with the stored session exactly as it does in
 * a browser, and every request it makes is counted from the first tick.
 *
 * The script is a no-op for the shim until `setupDashboard` has stored the
 * session seed in localStorage; the request counters are always installed.
 */

const SHIM = String.raw`
<script type="module">
  import { Signer, WebCryptoSigner } from "/vendor/firkin/index.js";

  const hex = (bytes) =>
    Array.from(bytes).map((b) => b.toString(16).padStart(2, "0")).join("");

  // --- Request counters, installed before the app can make any request -----
  {
    const { Firkin } = await import("/vendor/firkin/index.js");

    globalThis.__writes = [];
    globalThis.__reads = [];

    for (const name of ["ingestCar", "upload", "registrySet", "registrySetIf"]) {
      const original = Firkin.prototype[name];
      Firkin.prototype[name] = async function (...rest) {
        globalThis.__writes.push(name);
        return await original.apply(this, rest);
      };
    }

    for (
      const name of [
        "dagPull",
        "get",
        "registryNames",
        "registryList",
        "registryLookup",
      ]
    ) {
      const original = Firkin.prototype[name];
      Firkin.prototype[name] = async function (...rest) {
        globalThis.__reads.push(name);
        return await original.apply(this, rest);
      };
    }
  }

  // --- Ed25519 shim --------------------------------------------------------
  const stored = localStorage.getItem("__test_session_seed");

  if (stored) {
    const seed = Uint8Array.from(JSON.parse(stored));
    const signers = new Map();
    const pubBytes = new Map();
    const decoyFor = new Map();

    const ogImportKey = crypto.subtle.importKey.bind(crypto.subtle);
    const ogVerify = crypto.subtle.verify.bind(crypto.subtle);
    const ogSign = WebCryptoSigner.prototype.sign;

    crypto.subtle.importKey = async (format, data, alg, extractable, usages) => {
      if (alg?.name === "Ed25519") {
        const bytes = new Uint8Array(data);
        const id = hex(bytes);
        let decoy = decoyFor.get(id);

        if (!decoy) {
          decoy = await ogImportKey(
            "raw",
            new Uint8Array([0]),
            { name: "HMAC", hash: "SHA-256" },
            false,
            ["sign", "verify"],
          );
          decoyFor.set(id, decoy);
          pubBytes.set(decoy, bytes);
        }

        return decoy;
      }

      return ogImportKey(format, data, alg, extractable, usages);
    };

    crypto.subtle.verify = async (alg, key, signature, data) => {
      const name = typeof alg === "string" ? alg : alg?.name;
      const bytes = pubBytes.get(key);

      if (name === "Ed25519" && bytes) {
        const verifier = new Signer(new Uint8Array(32), bytes);
        return await verifier.verify(
          new Uint8Array(signature),
          new Uint8Array(data),
        );
      }

      return ogVerify(alg, key, signature, data);
    };

    let signerPromise;
    const signerFor = async (pubkeyBytes) => {
      const id = hex(pubkeyBytes);
      const known = signers.get(id);
      if (known) return known;

      signerPromise ??= Signer.fromSeed(seed);
      const signer = await signerPromise;
      if (hex(signer.pubkeyBytes) !== id) return undefined;

      signers.set(id, signer);
      return signer;
    };

    WebCryptoSigner.prototype.sign = async function (message) {
      const signer = this.pubkeyBytes
        ? await signerFor(this.pubkeyBytes)
        : undefined;

      if (signer) return await signer.sign(message);
      return await ogSign.call(this, message);
    };
  }
</script>
`;

const TYPES = {
  ".html": "text/html; charset=utf-8",
  ".js": "text/javascript; charset=utf-8",
  ".mjs": "text/javascript; charset=utf-8",
  ".css": "text/css; charset=utf-8",
  ".json": "application/json",
  ".map": "application/json",
  ".wasm": "application/wasm",
  ".webmanifest": "application/manifest+json",
  ".svg": "image/svg+xml",
  ".png": "image/png",
  ".jpg": "image/jpeg",
  ".jpeg": "image/jpeg",
  ".gif": "image/gif",
  ".webp": "image/webp",
  ".ico": "image/x-icon",
  ".woff": "font/woff",
  ".woff2": "font/woff2",
  ".ttf": "font/ttf",
  ".otf": "font/otf",
  ".txt": "text/plain; charset=utf-8",
  ".mp3": "audio/mpeg",
  ".wav": "audio/wav",
  ".ogg": "audio/ogg",
  ".pdf": "application/pdf",
};

/**
 * @param {string} root Directory to serve (`dist`).
 */
export function startStaticServer(root) {
  const base = root.replace(/\/+$/, "");

  const http = Deno.serve({ port: 0, hostname: "127.0.0.1" }, async (req) => {
    const url = new URL(req.url);
    let path = decodeURIComponent(url.pathname);

    if (path.endsWith("/")) path += "index.html";
    if (path === "") path = "/index.html";

    const file = `${base}${path}`;
    if (!file.startsWith(`${base}/`)) {
      return new Response("forbidden", { status: 403 });
    }

    let body;
    try {
      body = await Deno.readFile(file);
    } catch {
      // Mirror a directory server: `/dashboard` → `/dashboard/`.
      try {
        await Deno.stat(`${file}/index.html`);
        return Response.redirect(`${url.origin}${path}/`, 301);
      } catch {
        return new Response("missing", { status: 404 });
      }
    }

    const extension = file.slice(file.lastIndexOf("."));

    if (extension === ".html") {
      const html = new TextDecoder().decode(body);

      body = new TextEncoder().encode(
        html.replace("</head>", `${SHIM}</head>`),
      );
    }

    return new Response(req.method === "HEAD" ? null : body, {
      headers: {
        "Content-Type": TYPES[extension] ?? "application/octet-stream",
        "Cache-Control": "no-store",
      },
    });
  });

  const port = (http.addr as Deno.NetAddr).port;

  return { http, port, url: `http://127.0.0.1:${port}` };
}
