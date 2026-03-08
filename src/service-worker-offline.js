/// <reference lib="webworker" />

import { create as createCid } from "./common/cid.js";

const fileTreePromise = import("./file-tree.json", { with: { type: "json" } })
  .then((m) => m.default)
  .catch(() => null);

/** Media content types to ignore */
const MEDIA_CONTENT_TYPE = /^(audio|video)\//;

/** Multicodec code for raw binary content. */
const RAW_CODEC = 0x55;

const { searchParams } = new URL(location.href);
const CACHE_NAME = searchParams.get("cache-name") ?? "diffuse-offline";

const thyself =
  /** @type {ServiceWorkerGlobalScope & typeof globalThis} */ (/** @type {unknown} */ (self));

////////////////////////////////////////////
// INSTALL
////////////////////////////////////////////

self.addEventListener("install", () => {
  // Activate immediately without waiting for existing clients to close.
  thyself.skipWaiting();
});

////////////////////////////////////////////
// ACTIVATE
////////////////////////////////////////////

self.addEventListener("activate", (event) => {
  // Take control of all open clients right away.
  /** @type {ExtendableEvent} */ (event).waitUntil(thyself.clients.claim());
});

////////////////////////////////////////////
// FETCH
////////////////////////////////////////////

self.addEventListener("fetch", (_event) => {
  const event = /** @type {FetchEvent} */ (_event);
  const { request } = event;

  // Only intercept GET requests over http(s).
  if (request.method !== "GET") return;
  if (!request.url.startsWith("http")) return;

  event.respondWith(handleFetch(request));
});

////////////////////////////////////////////
// CONTENT-ADDRESSED CACHE
////////////////////////////////////////////

/**
 * @param {string} cid
 */
function cidUrl(cid) {
  return `https://diffuse.offline.worker/${cid}`;
}

/**
 * Opens the two caches used for content-addressed storage.
 *
 * - `<name>:index`   maps original request URL → CID string (text/plain)
 * - `<name>:content` maps `https://diffuse.offline.worker/<cid>` → full response (deduplicated)
 */
async function openCaches() {
  const [index, content] = await Promise.all([
    caches.open(CACHE_NAME + ":index"),
    caches.open(CACHE_NAME + ":content"),
  ]);
  return { index, content };
}

/**
 * Looks up a pathname in the pre-built file tree and returns its CID, or
 * `undefined` if the entry is absent or the tree failed to load.
 *
 * @param {string} pathname - e.g. "/components/foo.js"
 * @returns {Promise<string | undefined>}
 */
async function cidFromTree(pathname) {
  /** @type {Record<string, string> | null} */
  const tree = await fileTreePromise;
  if (!tree) return undefined;
  const key = pathname.replace(/^\//, "");
  return tree[key];
}

/**
 * Computes the CID of `response`'s body and writes it into the two-level cache.
 * The same content is stored only once, regardless of how many URLs reference it.
 *
 * Uses the pre-built file tree CID when available; falls back to hashing the
 * response body when the entry is missing from the tree.
 *
 * @param {Request} request
 * @param {Response} response - a clone; its body is fully consumed here
 */
async function store(request, response) {
  const { pathname } = new URL(request.url);
  const cid = await cidFromTree(pathname) ??
    await createCid(
      RAW_CODEC,
      new Uint8Array(await response.clone().arrayBuffer()),
    );
  const cidKey = cidUrl(cid);

  const caches = await openCaches();

  // Only store the content if we haven't seen this CID before
  if (!(await caches.content.match(cidKey))) {
    await caches.content.put(new Request(cidKey), response);
  }

  // Update the URL → CID map
  await caches.index.put(
    new Request(request.url),
    new Response(cid, { headers: { "content-type": "text/plain" } }),
  );
}

/**
 * Resolves the cached response for a request via the URL → CID index.
 *
 * @param {Request} request
 * @returns {Promise<Response | undefined>}
 */
async function lookup(request) {
  const caches = await openCaches();

  const indexEntry = await caches.index.match(request);
  if (!indexEntry) return undefined;

  const cid = await indexEntry.text();
  return caches.content.match(cidUrl(cid));
}

////////////////////////////////////////////
// HANDLER
////////////////////////////////////////////

/**
 * Network-first strategy with content-addressed caching.
 *
 * Online  → fetch from network, store response by CID, return it.
 * Offline → resolve the URL through the index, serve by CID from the content cache.
 *
 * Partial responses (206) are passed through without caching so that
 * range requests for audio streaming work as normal.
 *
 * @param {Request} request
 * @returns {Promise<Response>}
 */
async function handleFetch(request) {
  if (navigator.onLine) {
    try {
      return await fetchAndStore(request);
    } catch {}
  }

  const cached = await lookup(request);
  if (cached) return cached;

  return new Response(null, {
    status: 503,
    statusText: "Unavailable asset, not cached",
  });
}

/**
 * @param {Request} request
 */
async function fetchAndStore(request) {
  const response = await fetch(request);

  // Partial content (range requests) — return as-is, do not cache.
  if (response.status === 206) return response;

  // Skip caching audio/video.
  const contentType = response.headers.get("content-type") ?? "";
  if (MEDIA_CONTENT_TYPE.test(contentType)) return response;

  // Cache full successful responses, including opaque cross-origin ones.
  if (response.status === 200 || response.type === "opaque") {
    store(request, response.clone());
  }

  return response;
}
