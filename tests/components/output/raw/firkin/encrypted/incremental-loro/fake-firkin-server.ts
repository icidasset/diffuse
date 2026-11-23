// @ts-nocheck -- the fake server is deliberately untyped

import { carFile, parseCarBlocks } from "~/vendor/firkin/index.js";
import { CID } from "~/vendor/multiformats/cid.js";

const json = (body: unknown, status = 200) =>
  new Response(JSON.stringify(body), {
    status,
    headers: { "Content-Type": "application/json" },
  });

/**
 * A minimal Firkin server: registry with CAS, blobs, CAR ingest, DAG pull and a
 * hint relay. It ignores auth — the point is the protocol, not the signature
 * checks (those have unit tests).
 *
 * It is a real HTTP server (not an in-page `fetch` fake) because two devices
 * need separate browser profiles and therefore cannot share page state.
 */
export function startFakeFirkin() {
  const blobs = new Map<string, Uint8Array>();
  const registry = new Map<string, string>();
  const channels = new Map<string, Set<WebSocket>>();

  /** Artificial latency for data requests, to expose races with the UI. */
  let delayMs = 0;
  const setDelay = (ms: number) => {
    delayMs = ms;
  };
  const pause = () =>
    delayMs > 0
      ? new Promise((resolve) => setTimeout(resolve, delayMs))
      : Promise.resolve();

  const cors = (response: Response) => {
    response.headers.set("Access-Control-Allow-Origin", "*");
    response.headers.set("Access-Control-Allow-Headers", "*");
    response.headers.set(
      "Access-Control-Allow-Methods",
      "GET, HEAD, PUT, POST, OPTIONS",
    );
    return response;
  };

  const http = Deno.serve({ port: 0, hostname: "127.0.0.1" }, async (req) => {
    const url = new URL(req.url);
    const path = decodeURIComponent(url.pathname);

    if (req.method === "OPTIONS") return cors(new Response(null, { status: 204 }));

    if (path.startsWith("/v1/channel/")) {
      const name = path.slice("/v1/channel/".length);
      const { socket, response } = Deno.upgradeWebSocket(req);
      const peers = channels.get(name) ?? new Set<WebSocket>();

      peers.add(socket);
      channels.set(name, peers);

      socket.onmessage = (event) => {
        for (const peer of peers) {
          if (peer === socket || peer.readyState !== 1) continue;
          peer.send(event.data);
        }
      };
      socket.onclose = () => {
        peers.delete(socket);
        if (peers.size === 0) channels.delete(name);
      };

      return response;
    }

    if (delayMs > 0) await pause();

    if (path === "/v1/registry") {
      return cors(json({ namespaces: [] }));
    }

    if (path === "/v1/registry/lookup" && req.method === "POST") {
      const body = await req.json();
      const found: Record<string, string> = {};

      for (const name of body.names ?? []) {
        const cid = registry.get(`${body.namespace}/${name}`);
        if (cid) found[name] = cid;
      }

      return cors(json({ found }));
    }

    if (path.startsWith("/v1/registry/")) {
      const [namespace, name] = path.slice("/v1/registry/".length).split("/");

      if (req.method === "PUT") {
        const body = await req.json();
        const key = `${namespace}/${name}`;
        const current = registry.get(key) ?? null;
        const when = req.headers.get("If-Match");
        const none = req.headers.get("If-None-Match");

        if (
          (none === "*" && current !== null) ||
          (when !== null && when !== current)
        ) {
          return cors(json({ ok: false, current }, 412));
        }

        registry.set(key, body.cid);
        return cors(new Response(null, { status: 204 }));
      }

      const names: Record<string, string> = {};

      for (const [key, cid] of registry) {
        const prefix = `${namespace}/`;
        if (key.startsWith(prefix)) names[key.slice(prefix.length)] = cid;
      }

      return cors(json({ names }));
    }

    if (path.startsWith("/v1/blob/") && req.method === "PUT") {
      const cid = path.slice("/v1/blob/".length);
      blobs.set(cid, new Uint8Array(await req.arrayBuffer()));
      return cors(json({ status: "stored" }));
    }

    if (path.startsWith("/v1/blob/")) {
      const bytes = blobs.get(path.slice("/v1/blob/".length));
      if (!bytes) return cors(new Response("missing", { status: 404 }));

      return cors(
        new Response(bytes, {
          headers: { "Content-Type": "application/octet-stream" },
        }),
      );
    }

    if (path === "/v1/car" && req.method === "POST") {
      const blocks = parseCarBlocks(
        new Uint8Array(await req.arrayBuffer()),
      );
      let stored = 0;

      for (const [cid, bytes] of blocks) {
        if (!blobs.has(cid)) stored++;
        blobs.set(cid, bytes);
      }

      return cors(
        json({
          blocks: blocks.size,
          stored,
          already: blocks.size - stored,
          failed_total: 0,
          failed: [],
        }),
      );
    }

    if (path.startsWith("/v1/dag/pull/") && req.method === "POST") {
      const root = path.slice("/v1/dag/pull/".length);
      if (!blobs.has(root)) return cors(new Response("missing", { status: 404 }));

      const car = carFile(
        CID.parse(root),
        [...blobs].map(([cid, bytes]) => [CID.parse(cid), bytes]),
      );

      return cors(
        new Response(car, {
          headers: { "Content-Type": "application/vnd.ipld.car" },
        }),
      );
    }

    return cors(new Response(`unexpected ${req.method} ${path}`, { status: 404 }));
  });

  const port = (http.addr as Deno.NetAddr).port;

  /** Drop every open hint socket, as a flaky network would. */
  const dropChannels = () => {
    for (const peers of channels.values()) {
      for (const socket of peers) socket.close();
    }
    channels.clear();
  };

  return { http, port, blobs, registry, dropChannels, setDelay };
}
