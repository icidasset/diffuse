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

import * as CID from "~/common/cid.js";
import { effect } from "~/common/signal.js";

// When the service worker takes control (clients.claim()), the page is about
// to reload (see default-layout.js sw-activated handler). Any fetch() calls
// in flight at that moment will be cancelled by the navigation and throw a
// NetworkError. We detect the controller change here so we can suppress those
// spurious errors rather than flashing an error UI before the reload.
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
 * @typedef {{ html?: string; uri?: string; cid?: string; id: string; name: string; $type: string }} LoadableItem
 */

/**
 * @typedef {object} LoaderConfig
 * @property {string} $type - The atproto $type
 * @property {string} label - Human-readable label for error messages (e.g. "Facet", "Theme")
 * @property {() => { collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: LoadableItem[] }> }} source - The collection source
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
  const cid = docUrl.searchParams.get("cid");
  const name = docUrl.searchParams.get("name");
  const uri = docUrl.searchParams.get("uri");
  const path = docUrl.searchParams.get("path");

  const containerNull = document.querySelector("#container");
  if (!containerNull) throw new Error("Container not found");

  const container = /** @type {HTMLDivElement} */ (containerNull);

  /** @type {string | null} */
  let loadedCid = null;

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
      if (col.state !== "loaded") return;
      const collection = col.data;

      if (id) {
        item = collection.find((c) => c.id === id);
        loader = "id";
      } else if (cid) {
        item = collection.find((c) => c.cid === cid);
        loader = "cid";
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
    });

    if (item.cid === loadedCid) return;

    loadedCid = item.cid ?? null;
    config.render(item);
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
 * Ensures the item has HTML loaded. If it has a URI but no HTML,
 * fetches the HTML and computes the CID.
 *
 * @template {{ html?: string; uri?: string; cid?: string }} T
 * @param {T} item
 * @returns {Promise<T>}
 */
export async function ensureHTML(item) {
  if (!item.html && item.uri) {
    const html = await loadURI(item.uri);
    const cid = await CID.create(0x55, new TextEncoder().encode(html));

    item.html = html;
    item.cid = cid;
  }

  return item;
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
