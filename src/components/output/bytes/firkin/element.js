import { ApiError } from "@tokono.ma/firkin";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";
import { getSessionClient, watchSession } from "~/common/firkin/session.js";

import { CODEC_RAW, NOT_CONNECTED, registryNamespace } from "./constants.js";

/**
 * @import {OutputElement, OutputManager} from "@specs/components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Stores output as raw Firkin blobs, with one account-registry pointer per
 * collection.
 *
 * This is the generic Firkin counterpart to the S3/Dropbox byte outputs: it
 * knows nothing about WNFS, passkeys or a particular sync strategy. Pair it with
 * a transformer (DASL sync, Automerge, Subduction, …) for local-first behavior.
 *
 * @implements {OutputElement<Uint8Array | undefined>}
 */
class FirkinOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/bytes/firkin";

  /** @type {OutputManager<Uint8Array | undefined>} */
  #manager;

  // SIGNALS

  #isOnline = signal(navigator.onLine);
  #connected = signal(false);

  /** @type {(() => void) | undefined} */
  #unwatchSession;

  constructor() {
    super();

    this.#manager = outputManager({
      collaborations: {
        empty: () => undefined,
        get: () => this.#get("collaborations"),
        put: (data) => this.#put("collaborations", data),
      },
      facets: {
        empty: () => undefined,
        get: () => this.#get("facets"),
        put: (data) => this.#put("facets", data),
      },
      init: () => this.whenConnected(),
      playlistItems: {
        empty: () => undefined,
        get: () => this.#get("playlistItems"),
        put: (data) => this.#put("playlistItems", data),
      },
      settings: {
        empty: () => undefined,
        get: () => this.#get("settings"),
        put: (data) => this.#put("settings", data),
      },
      tracks: {
        empty: () => undefined,
        get: () => this.#get("tracks"),
        put: (data) => this.#put("tracks", data),
      },
    });

    this.collaborations = this.#manager.collaborations;
    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
    this.tracks = this.#manager.tracks;
  }

  ready = computed(() => this.#connected.value && this.#isOnline.value);

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    this.replicateSavedData(this.#manager);

    super.connectedCallback();

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);

    // Turn ready as soon as the shared account session is available (the
    // Connect → Firkin page may sign in while this output is already mounted).
    this.#unwatchSession = watchSession(() => this.#refreshConnected());
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);

    this.#unwatchSession?.();
    this.#unwatchSession = undefined;

    super.disconnectedCallback();
  }

  #offline = () => this.#isOnline.set(false);
  #online = () => {
    this.#isOnline.set(true);
    void this.#refreshConnected();
  };

  async #refreshConnected() {
    try {
      const client = await getSessionClient();
      if (!client) {
        this.#connected.value = false;
        return;
      }

      try {
        // A cheap authenticated read proves the session (and account) works.
        await client.rb.registryList();
        this.#connected.value = true;
      } catch (err) {
        // Unauthorized/forbidden means the account is unregistered or the
        // delegation was revoked; other errors are treated optimistically.
        this.#connected.value = !(
          err instanceof ApiError && (err.status === 401 || err.status === 403)
        );
      }
    } catch {
      this.#connected.value = false;
    }
  }

  // GET & PUT

  /** @param {string} name */
  #get = async (name) => {
    const client = await getSessionClient();
    if (!client) return undefined;

    const namespace = registryNamespace(this.namespace);
    const cid = await client.rb.registryGet(namespace, name);
    if (!cid) return undefined;

    try {
      return await client.rb.get(cid);
    } catch (err) {
      // A dangling registry pointer is equivalent to a missing collection.
      if (err instanceof ApiError && err.status === 404) return undefined;
      throw err;
    }
  };

  /**
   * @param {string} name
   * @param {Uint8Array | undefined} data
   */
  #put = async (name, data) => {
    const client = await getSessionClient();
    if (!client) throw new Error(NOT_CONNECTED);
    if (!data) return;

    const rb = client.rb;
    const namespace = registryNamespace(this.namespace);

    const previous = await rb.registryGet(namespace, name);
    const { cid } = await rb.upload(data, { codec: CODEC_RAW });
    await rb.registrySet(namespace, name, cid);

    // Drop the replaced blob when no other collection points at it.
    if (previous && previous !== cid) {
      const names = await rb.registryNames(namespace);
      if (!names || !Object.values(names).includes(previous)) {
        await rb.delete(previous).catch(() => {});
      }
    }
  };
}

export default FirkinOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = FirkinOutput;
export const NAME = "dob-firkin";

defineElement(NAME, FirkinOutput);
