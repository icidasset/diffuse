import * as IDB from "idb-keyval";

import { computed, signal } from "~/common/signal.js";
import { BroadcastedOutputElement, outputManager } from "../../common.js";
import { defineElement } from "~/common/element.js";
import { generatePKCEPair } from "~/components/input/dropbox/common.js";
import { APP_KEY, PKCE_VERIFIER_KEY } from "./constants.js";

const STORAGE_PREFIX = "diffuse/output/bytes/dropbox";

/**
 * @import {ProxiedActions} from "~/common/worker.d.ts"
 * @import {OutputElement, OutputManager} from "@specs/components/output/types.d.ts"
 * @import {DropboxOutputElement, DropboxOutputWorkerActions} from "@specs/components/output/bytes/dropbox/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Stores user-data (tracks, playlists, settings, facets) as raw bytes in
 * the Dropbox app folder. Uses the authorization-code-with-PKCE OAuth flow
 * to obtain a long-lived refresh token, which automatically renews the
 * short-lived access token (~4 h) via the shared `getAccessToken` helper.
 *
 * @implements {OutputElement<Uint8Array | undefined>}
 * @implements {DropboxOutputElement}
 */
class DropboxOutput extends BroadcastedOutputElement {
  static NAME = "diffuse/output/bytes/dropbox";
  static WORKER_URL = "components/output/bytes/dropbox/worker.js";

  #manager;

  constructor() {
    super();

    /** @type {ProxiedActions<DropboxOutputWorkerActions>} */
    this.proxy = this.workerProxy();

    /** @type {OutputManager<Uint8Array | undefined>} */
    this.#manager = outputManager({
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

    this.facets = this.#manager.facets;
    this.playlistItems = this.#manager.playlistItems;
    this.settings = this.#manager.settings;
    this.tracks = this.#manager.tracks;
  }

  // SIGNALS

  #isOnline = signal(navigator.onLine);

  // STATE

  ready = computed(() => {
    return this.#refreshToken.value !== undefined && this.#isOnline.value;
  });

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    this.replicateSavedData(this.#manager);

    super.connectedCallback();

    /** @type {string | undefined} */
    const stored = await IDB.get(`${STORAGE_PREFIX}/refresh_token`);
    if (stored) this.#refreshToken.value = stored;

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);
  }

  #offline = () => this.#isOnline.set(false);
  #online = () => this.#isOnline.set(true);

  // REFRESH TOKEN

  #refreshToken = signal(/** @type {string | undefined} */ (undefined));

  refreshToken = this.#refreshToken.get;

  /** @returns {Promise<string | undefined>} */
  async getRefreshToken() {
    if (!this.#refreshToken.value) {
      /** @type {string | undefined} */
      const stored = await IDB.get(`${STORAGE_PREFIX}/refresh_token`);
      if (stored) this.#refreshToken.value = stored;
      return stored;
    }

    return this.#refreshToken.value;
  }

  /**
   * @param {string} token
   */
  async setRefreshToken(token) {
    this.#refreshToken.value = token;
    await IDB.set(`${STORAGE_PREFIX}/refresh_token`, token);
  }

  async unsetRefreshToken() {
    this.#refreshToken.value = undefined;
    await IDB.del(`${STORAGE_PREFIX}/refresh_token`);
  }

  // AUTHORIZE

  /**
   * Starts the authorization-code-with-PKCE OAuth flow for the output
   * Dropbox app. The verifier is stored under a separate key so the
   * callback page can distinguish it from the input/upload flow.
   */
  async authorize() {
    localStorage.setItem(
      "oauth/callback/redirect_path",
      location.pathname + location.search,
    );

    const { verifier, challenge } = await generatePKCEPair();
    localStorage.setItem(PKCE_VERIFIER_KEY, verifier);

    const params = new URLSearchParams({
      response_type: "code",
      client_id: APP_KEY,
      redirect_uri: location.origin + "/oauth/callback/",
      token_access_type: "offline",
      code_challenge: challenge,
      code_challenge_method: "S256",
    });

    location.assign(`https://www.dropbox.com/oauth2/authorize?${params}`);
  }

  // GET & PUT

  /** @param {string} name */
  #get = async (name) => {
    const refreshToken = await this.getRefreshToken();
    if (!refreshToken) return undefined;
    return this.proxy.get({ refreshToken, name: this.#cat(name) });
  };

  /** @param {string} name; @param {any} data */
  #put = async (name, data) => {
    const refreshToken = await this.getRefreshToken();
    if (!refreshToken) return undefined;
    return this.proxy.put({ refreshToken, data, name: this.#cat(name) });
  };

  // 🛠️

  /** @param {string} name */
  #cat(name) {
    const ns = this.namespace;
    return `${ns?.length ? ns + "/" : ""}${name}`;
  }
}

export default DropboxOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = DropboxOutput;
export const NAME = "dob-dropbox";

defineElement(NAME, DropboxOutput);
