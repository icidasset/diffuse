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

import * as CID from "@common/cid.js";
import { effect } from "@common/signal.js";

/**
 * @import {SignalReader} from "@common/signal.d.ts"
 */

/**
 * @typedef {{ html?: string; uri?: string; cid?: string; id: string; name: string; $type: string }} LoadableItem
 */

/**
 * @typedef {object} LoaderConfig
 * @property {string} $type - The atproto $type
 * @property {string} label - Human-readable label for error messages (e.g. "Facet", "Theme")
 * @property {() => { collection: SignalReader<LoadableItem[]>; state: SignalReader<"loading" | "loaded" | "sleeping"> }} source - The collection source
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

  effect(async () => {
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
      const collection = source.collection();
      if (source.state() !== "loaded") return;

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
    await ensureHTML(item).catch((err) => {
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
  container.innerHTML = `
    <div class="diffuse">
      <div class="flex">
        <i class="ph-fill ph-warning"></i>
        <span>${error}</span>
      </div>
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

  return value.html ?? "";
}

/**
 * @param {string} url
 * @returns {Promise<string>}
 */
async function httpLoader(url) {
  return fetch(url).then((res) => res.text());
}
