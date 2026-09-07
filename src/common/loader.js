import * as URI from "fast-uri";
import * as TID from "@atcute/tid";
import { Client, ok, simpleFetchHandler } from "@atcute/client";
import {
  CompositeDidDocumentResolver,
  LocalActorResolver,
  PlcDidDocumentResolver,
  WebDidDocumentResolver,
  XrpcHandleResolver,
} from "@atcute/identity-resolver";

import { effect } from "~/common/signal.js";
import {
  decodeBlocks,
  parseCar,
  resolveRoot,
  rewriteModuleImports,
  tileResourceEntries,
} from "./tiles.js";

// When the service worker takes control (clients.claim()), the page is about
// to reload (see service-worker-loader.js reloadForNewController, which fires
// on both the "sw-activated" message and the "controllerchange" event). Any
// fetch() calls in flight at that moment will be cancelled by the navigation
// and throw a NetworkError. We detect the controller change here so we can
// suppress those spurious errors rather than flashing an error UI before the
// reload.
let swControllerChanging = false;

if ("serviceWorker" in navigator) {
  navigator.serviceWorker.addEventListener("controllerchange", () => {
    swControllerChanging = true;
  });
}

/**
 * @import {SignalReader} from "~/common/signal.d.ts"
 */

/**
 * @typedef {{ resources: Record<string, { src: unknown; "content-type"?: string }>; blocks: Map<string, Uint8Array> }} TileLink
 *
 * @typedef {{ html?: string; uri?: string; cid?: string; resources?: unknown; blocks?: Record<string, unknown>; tile?: TileLink; id: string; name: string; $type: string }} LoadableItem
 *
 * `html` and `cid` are internal, non-persisted fields populated while loading:
 * `html` is the facet's resolved index document (for fragment injection), `cid`
 * is the tile root resource's CID (for content-addressed verification), and
 * `tile` carries the resolved resources + blocks so loaders can serve the
 * tile's absolute-path resources as Blob URLs.
 */

/**
 * @typedef {object} LoaderConfig
 * @property {string} $type - The atproto $type
 * @property {string} label - Human-readable label for error messages (e.g. "Facet", "Theme")
 * @property {() => { collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: LoadableItem[] } | { state: "error" }> }} source - The collection source
 * @property {(item: LoadableItem) => void} render - Renders the loaded item
 */

/**
 * Sets up the full loader effect: reads URL params, resolves the item
 * from the collection or creates a temporary one, ensures HTML is loaded,
 * and calls the render callback.
 *
 * @param {LoaderConfig} config
 */
export function createLoader(config) {
  const docUrl = new URL(document.location.href);

  const id = docUrl.searchParams.get("id");
  const name = docUrl.searchParams.get("name");
  const uri = docUrl.searchParams.get("uri");
  const path = docUrl.searchParams.get("path");

  const containerNull = document.querySelector("#container");
  if (!containerNull) throw new Error("Container not found");

  const container = /** @type {HTMLDivElement} */ (containerNull);

  /** @type {string | null} */
  let loadedId = null;

  /** @type {string | null} */
  let loader = null;

  effect(() => {
    /** @type {LoadableItem | undefined} */
    let item = undefined;

    if (path) {
      item = {
        $type: config.$type,
        id: TID.now(),
        name: "temporary",
        uri: `diffuse://${path}`,
      };

      loader = "path";
    } else if (uri) {
      item = {
        $type: config.$type,
        id: TID.now(),
        name: "temporary",
        uri,
      };

      loader = "uri";
    } else {
      const source = config.source();
      const col = source.collection();
      if (col.state === "error") {
        return renderError(container, `Failed to load ${config.label.toLowerCase()}`);
      }
      if (col.state !== "loaded") return;
      const collection = col.data;

      if (id) {
        item = collection.find((c) => c.id === id);
        loader = "id";
      } else if (name) {
        item = collection.find((c) => c.name === name);
        loader = "name";
      }
    }

    if (!loader) {
      return renderError(container, "No loader specified");
    } else if (!item) {
      return renderError(container, `${config.label} not found`);
    }

    // Make sure HTML is loaded when a URI is specified
    ensureHTML(item).catch((err) => {
      if (swControllerChanging) return;
      renderError(container, `Failed to load URI: ${item.uri}`, {
        context: err,
        throw: true,
      });
    }).then(() => {
      if (item.id === loadedId) return;
      loadedId = item.id ?? null;
      config.render(item);
    })
  });
}

/**
 * @param {string} uri
 */
export async function loadURI(uri) {
  const u = URI.parse(uri);

  switch (u.scheme) {
    case "at":
      return atprotoLoader(uri);
    case "diffuse":
      return httpLoader(uri.replace(/^diffuse:\/\//, ""));
    case "http":
    case "https":
      return httpLoader(uri);
    default:
      throw new Error(`Unsupported scheme: ${u.scheme}`);
  }
}

/**
 * @typedef {{ html: string; cid?: string; resources: Record<string, { src: unknown; "content-type"?: string }>; blocks: Map<string, Uint8Array> }} MaterializedTile
 */

/**
 * Ensures the item has HTML loaded. Tiles (inline `resources`+`blocks`, or a
 * `.tile` CAR referenced by `uri`) resolve their `/` resource; for those,
 * `item.cid` is set to the root resource's CID for content-addressed integrity
 * and `item.tile` carries the resolved resources + blocks so callers can serve
 * the tile's absolute-path resources. Otherwise falls back to loading `uri` as
 * plain HTML.
 *
 * @template {{ html?: string; uri?: string; cid?: string; resources?: unknown; blocks?: Record<string, unknown>; tile?: TileLink }} T
 * @param {T} item
 * @returns {Promise<T>}
 */
export async function ensureHTML(item) {
  if (item.html) return item;

  const tile = await materializeTile(item);
  if (tile) {
    item.html = tile.html;
    item.cid = tile.cid;
    item.tile = {
      resources: tile.resources,
      blocks: tile.blocks,
    };
    return item;
  }

  if (item.uri) {
    item.html = await loadURI(item.uri);
  }

  return item;
}

/**
 * Resolves a facet's full tile content: the root HTML plus its resolved
 * resources map and blocks. Handles inline tiles (a `resources` map plus a
 * `blocks` map) and `.tile` CARs referenced by `uri`.
 *
 * @param {{ uri?: string; resources?: unknown; blocks?: Record<string, unknown> }} item
 * @returns {Promise<MaterializedTile | undefined>}
 */
async function materializeTile(item) {
  if (item.resources) {
    const resources = /** @type {Record<string, { src: unknown; "content-type"?: string }>} */ (item.resources);
    const blocks = await decodeBlocks(item.blocks);
    const root = resolveRoot(resources, blocks);
    if (!root) return undefined;
    return { html: root.html, cid: root.cid, resources, blocks };
  }

  if (item.uri?.endsWith(".tile")) {
    return await loadTileURI(item.uri);
  }

  return undefined;
}

/**
 * Fetches a `.tile` CAR and resolves its `/` resource plus its resources map
 * (per the DASL "Tiles in CAR" convention, the CAR header is the MASL
 * manifest).
 *
 * @param {string} uri
 * @returns {Promise<MaterializedTile | undefined>}
 */
async function loadTileURI(uri) {
  const u = URI.parse(uri);
  if (u.scheme === "diffuse") uri = uri.replace(/^diffuse:\/\//, "");

  const res = await fetch(uri);
  const bytes = new Uint8Array(await res.arrayBuffer());
  const { manifest, blocks } = parseCar(bytes);
  const resources = /** @type {Record<string, { src: unknown; "content-type"?: string }>} */ (manifest.resources);
  const root = resolveRoot(resources, blocks);
  if (!root) return undefined;
  return { html: root.html, cid: root.cid, resources, blocks };
}

/**
 * Resolves a facet's HTML and its resolved tile resources (for later Blob-URL
 * linking). Plain-HTML/URI facets resolve with empty resources.
 *
 * @param {{ html?: string; uri?: string; resources?: unknown; blocks?: Record<string, unknown> }} facet
 * @returns {Promise<{ html: string; resources: Record<string, { src: unknown; "content-type"?: string }>; blocks: Map<string, Uint8Array> } | undefined>}
 */
export async function resolveFacetTile(facet) {
  if (facet.html) {
    return { html: facet.html, resources: {}, blocks: new Map() };
  }

  const tile = await materializeTile(facet);
  if (tile) {
    return { html: tile.html, resources: tile.resources, blocks: tile.blocks };
  }

  if (facet.uri) {
    return { html: await loadURI(facet.uri), resources: {}, blocks: new Map() };
  }

  return undefined;
}

/**
 * Resolves a facet's HTML as a string, handling plain HTML and the two tile
 * forms (inline `resources`+`blocks`, and `.tile` CARs referenced by `uri`).
 *
 * @param {{ html?: string; uri?: string; resources?: unknown; blocks?: Record<string, unknown> }} facet
 * @returns {Promise<string>}
 */
export async function resolveFacetHTML(facet) {
  return (await resolveFacetTile(facet))?.html ?? "";
}

/** @param {string} path @param {string | undefined} contentType */
function isModuleResource(path, contentType) {
  const type = (contentType ?? "").toLowerCase();
  if (type.includes("javascript") || type.includes("ecmascript") || type === "module") {
    return true;
  }
  return /\.(cjs|mjs|js|jsx|ts|tsx)$/.test(path);
}

/**
 * @param {Uint8Array | string} content
 * @param {string | undefined} contentType
 * @param {boolean} isModule
 */
function createBlobURL(content, contentType, isModule) {
  const type = contentType ?? (isModule ? "text/javascript" : "application/octet-stream");
  return URL.createObjectURL(new Blob([/** @type {BlobPart} */ (content)], { type }));
}

/**
 * Serves a tile's absolute-path resources as Blob URLs and rewrites references
 * to them. Two kinds of rewriting happen, both limited to absolute paths (a
 * leading `/`) matching a tile resource: (1) DOM attributes (`src`/`href`/
 * `srcset`) are pointed at the Blob URLs; (2) module resources rewrite their
 * own `import`/`export … from`/`import()` absolute specifiers to the target's
 * Blob URL (import maps are ignored for `blob:` origins). Relative references
 * are left untouched, so they keep their meaning against the Diffuse build root.
 *
 * @param {ParentNode} container - An element or detached fragment whose absolute
 *   resource URLs (e.g. `/styles.css`) are rewritten to Blob URLs.
 * @param {Record<string, { src: unknown; "content-type"?: string }>} resources
 * @param {Map<string, Uint8Array>} blocks
 */
export function linkTileResources(container, resources, blocks) {
  const entries = tileResourceEntries(resources, blocks);
  if (entries.size === 0) return;

  /** @type {Map<string, string>} */
  const urls = new Map();
  for (const [path, entry] of entries) {
    if (path === "/") continue;
    const isModule = isModuleResource(path, entry.contentType);
    urls.set(path, createBlobURL(entry.bytes, entry.contentType, isModule));
  }
  if (urls.size === 0) return;

  // Module resources may import each other by absolute path. Blob origins are
  // excluded from import maps, so rewrite the importing module's specifiers to
  // the target's Blob URL directly. Iterate to a fixpoint so module → module
  // chains all point at the final (already-rewritten) Blob URLs.
  const modulePaths = [...urls.keys()].filter((path) =>
    isModuleResource(path, entries.get(path)?.contentType ?? undefined)
  );
  if (modulePaths.length) {
    const decoder = new TextDecoder();
    let changed = true;
    let guard = modulePaths.length + 1;
    while (changed && guard-- > 0) {
      changed = false;
      for (const path of modulePaths) {
        const entry = entries.get(path);
        if (!entry) continue;
        const source = decoder.decode(entry.bytes);
        const rewritten = rewriteModuleImports(
          source,
          (specifier) => urls.get(specifier),
        );
        if (rewritten !== source) {
          urls.set(path, createBlobURL(rewritten, entry.contentType, true));
          changed = true;
        }
      }
    }
  }

  for (const el of container.querySelectorAll("*")) {
    for (const attr of ["src", "href"]) {
      const value = el.getAttribute(attr);
      const linked = value ? urls.get(value) : undefined;
      if (linked !== undefined) el.setAttribute(attr, linked);
    }

    const srcset = el.getAttribute("srcset");
    if (srcset) {
      const rewritten = srcset.split(",").map((part) => {
        const bits = part.trim().split(/\s+/);
        const linked = urls.get(bits[0]);
        if (linked !== undefined) bits[0] = linked;
        return bits.join(" ");
      }).join(",");
      el.setAttribute("srcset", rewritten);
    }
  }
}

/**
 * @param {HTMLElement} container
 * @param {string} error
 * @param {{ context?: Error; throw?: boolean }} [options]
 */
export function renderError(container, error, options) {
  document.querySelector("#diffuse-loader")?.classList.add("loaded");
  container.classList.add("has-loaded");
  container.innerHTML = `
    <div class="diffuse">
      <a href="./" class="flex" style="color: inherit; text-decoration: none;">
        <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" fill="currentColor" viewBox="0 0 256 256"><path d="M216,40H40A16,16,0,0,0,24,56V200a16,16,0,0,0,16,16h64a8,8,0,0,0,7.59-5.47l14.83-44.48L163,151.43a8.07,8.07,0,0,0,4.46-4.46l14.62-36.55,44.48-14.83A8,8,0,0,0,232,88V56A16,16,0,0,0,216,40ZM117,152.57a8,8,0,0,0-4.62,4.9L98.23,200H40V160.69l46.34-46.35a8,8,0,0,1,11.32,0l32.84,32.84Zm115-30.84V200a16,16,0,0,1-16,16H137.73a8,8,0,0,1-7.59-10.53l7.94-23.8a8,8,0,0,1,4.61-4.9l35.77-14.31,14.31-35.77a8,8,0,0,1,4.9-4.61l23.8-7.94A8,8,0,0,1,232,121.73Z"></path></svg>
        <span style="font-size: var(--fs-base); font-weight: 700;">${error}</span>
      </a>
    </div>
  `;

  if (options?.throw) {
    throw options.context ?? new Error(error);
  }
}

////////////////////////////////////////////
// 🛠️ | LOADERS
////////////////////////////////////////////

/**
 * @param {string} uri
 * @returns {Promise<string>}
 */
async function atprotoLoader(uri) {
  const parts = uri.replace(/at:\/\//, "").split("/");
  const [repo, collection, rkey] = parts;

  const resolver = new LocalActorResolver({
    handleResolver: new XrpcHandleResolver({
      serviceUrl: "https://public.api.bsky.app",
    }),
    didDocumentResolver: new CompositeDidDocumentResolver({
      methods: {
        plc: new PlcDidDocumentResolver(),
        web: new WebDidDocumentResolver(),
      },
    }),
  });

  const identity = await resolver.resolve(
    /** @type {import("@atcute/lexicons/syntax").ActorIdentifier} */ (repo),
  );

  const rpc = new Client({
    handler: simpleFetchHandler({ service: identity.pds }),
  });

  /** @type {any} */
  const { value } = await ok(
    /** @type {any} */ (rpc).get("com.atproto.repo.getRecord", {
      params: { repo: identity.did, collection, rkey },
    }),
  );

  if (value.html) {
    return value.html;
  }

  if (value.resources) {
    const content = await materializeTile(value);
    if (content) return content.html;
  }

  if (value.uri) {
    return loadURI(value.uri);
  }

  return "";
}

/**
 * @param {string} url
 * @returns {Promise<string>}
 */
async function httpLoader(url) {
  return fetch(url).then((res) => res.text());
}
