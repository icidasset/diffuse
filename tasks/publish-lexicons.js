// Publishes the Diffuse lexicons (including the `sh.diffuse.atproto.space`
// space declaration) as `com.atproto.lexicon.schema` records on an atproto PDS.
//
// This mirrors the atproto reference app's lexicon-publishing step. Note that a
// `space:` scope only resolves if the PDS's lexicon authority resolves the
// `sh.diffuse.*` namespace from the target account's DID.
//
// Usage:
//   deno run -A tasks/publish-lexicons.js \
//     https://spaces-alpha.host.bsky.network \
//     toko.spaces-alpha.bsky.network \
//     "<login-password>"

const LEXICON_COLLECTION = "com.atproto.lexicon.schema";

const [pds, handle, password] = Deno.args;

if (!pds || !handle || !password) {
  console.error(
    "Usage: deno run -A tasks/publish-lexicons.js <pds> <handle> <login-password>",
  );
  Deno.exit(1);
}

/** Recursively collect lexicon JSON files under a directory. */
async function collect(dir) {
  const out = [];

  for await (const entry of Deno.readDir(dir)) {
    const path = `${dir}/${entry.name}`;

    if (entry.isDirectory) {
      out.push(...(await collect(path)));
    } else if (entry.isFile && entry.name.endsWith(".json")) {
      out.push(path);
    }
  }

  return out;
}

async function xrpc(method, init) {
  const res = await fetch(`${pds}/xrpc/${method}`, init);
  const text = await res.text();

  if (!res.ok) {
    throw new Error(`${method} failed (${res.status}): ${text.slice(0, 400)}`);
  }

  try {
    return JSON.parse(text);
  } catch {
    return text;
  }
}

const login = await xrpc("com.atproto.server.createSession", {
  method: "POST",
  headers: { "content-type": "application/json" },
  body: JSON.stringify({ identifier: handle, password }),
});

const accessJwt = /** @type {string} */ (login.accessJwt);
const did = /** @type {string} */ (login.did);

for (const file of await collect("lexicons")) {
  const doc = JSON.parse(await Deno.readTextFile(file));

  await xrpc("com.atproto.repo.putRecord", {
    method: "POST",
    headers: {
      authorization: `Bearer ${accessJwt}`,
      "content-type": "application/json",
    },
    body: JSON.stringify({
      repo: did,
      collection: LEXICON_COLLECTION,
      rkey: doc.id,
      record: doc,
    }),
  });

  console.log(`published: ${doc.id}`);
}
