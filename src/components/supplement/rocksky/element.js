import { md5 } from "@noble/hashes/legacy.js";
import { bytesToHex, utf8ToBytes } from "@noble/hashes/utils.js";

import { getSession } from "@atcute/oauth-browser-client";

import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ScrobbleElement} from "../types.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

const ROCKSKY_API_URL = "https://audioscrobbler.rocksky.app/2.0/";
const ATPROTO_DID_KEY = "diffuse/output/raw/atproto/did";
const STORAGE_KEY = "diffuse/supplement/rocksky/session";

const DEFAULT_API_KEY = "d21bdb464bd5e92c4dbbe814a5a9a8a4";
const DEFAULT_API_SECRET = "4a9d15e43ad1623ee7f9dc6b12d6ba08";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ScrobbleElement}
 */
class RockskyScrobbler extends BroadcastableDiffuseElement {
  static NAME = "diffuse/supplement/rocksky";

  get #apiKey() {
    return this.getAttribute("api-key") ?? DEFAULT_API_KEY;
  }

  get #apiSecret() {
    return this.getAttribute("api-secret") ?? DEFAULT_API_SECRET;
  }

  // SIGNALS

  #handle = signal(/** @type {string | null} */ (null));
  #sessionKey = signal(/** @type {string | null} */ (null));
  #isAuthenticating = signal(false);

  // STATE

  handle = this.#handle.get;
  isAuthenticated = computed(() => this.#sessionKey.value !== null);
  isAuthenticating = this.#isAuthenticating.get;

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        nowPlaying: { strategy: "leaderOnly", fn: this.nowPlaying },
        scrobble: { strategy: "leaderOnly", fn: this.scrobble },

        setHandle: { strategy: "replicate", fn: this.#handle.set },
        setSession: { strategy: "replicate", fn: this.#sessionKey.set },
      });

      if (actions) {
        this.nowPlaying = actions.nowPlaying;
        this.scrobble = actions.scrobble;

        this.#handle.set = actions.setHandle;
        this.#sessionKey.set = actions.setSession;
      }
    }

    super.connectedCallback();

    this.#tryRestore();
  }

  async #tryRestore() {
    await this.whenConnected();

    const stored = localStorage.getItem(STORAGE_KEY);

    if (stored) {
      try {
        const { key, name: handle } = JSON.parse(stored);
        if (await this.isLeader()) {
          this.#sessionKey.set(key);
          this.#handle.set(handle);
        } else {
          this.#sessionKey.value = key;
          this.#handle.value = handle;
        }
      } catch {
        localStorage.removeItem(STORAGE_KEY);
      }
    }
  }

  // AUTH

  /**
   * Sign in to Rocksky using the existing AT Protocol session.
   * Exchanges the AT Protocol access token for a Rocksky audioscrobbler session key.
   */
  async signIn() {
    const did = localStorage.getItem(ATPROTO_DID_KEY);
    if (!did) throw new Error("rocksky: no AT Protocol session found");

    this.#isAuthenticating.set(true);
    try {
      const session = await getSession(
        /** @type {`did:${string}:${string}`} */ (did),
      );
      const accessToken = session.token.access;

      const data = await this.#call("auth.getMobileSession", {
        username: did,
        password: accessToken,
      });
      this.#setSession(data.session);
    } catch (err) {
      console.warn("rocksky: failed to authenticate", err);
      throw err;
    } finally {
      this.#isAuthenticating.set(false);
    }
  }

  /**
   * Clear the stored session.
   */
  signOut() {
    this.#sessionKey.set(null);
    this.#handle.set(null);
    localStorage.removeItem(STORAGE_KEY);
  }

  /** @param {{ key: string, name: string }} session */
  #setSession({ key, name: handle }) {
    this.#sessionKey.set(key);
    this.#handle.set(handle);
    localStorage.setItem(STORAGE_KEY, JSON.stringify({ key, name: handle }));
  }

  // SCROBBLE ACTIONS

  /**
   * @param {Track} track
   */
  async nowPlaying(track) {
    const tags = track.tags ?? {};
    /** @type {Record<string, string>} */
    const params = {};

    if (tags.title) params.track = tags.title;
    if (tags.artist) params.artist = tags.artist;
    if (tags.album) params.album = tags.album;
    if (tags.albumartist) params.albumArtist = tags.albumartist;
    if (tags.track?.no != null) params.trackNumber = String(tags.track.no);
    if (track.stats?.duration != null) {
      params.duration = String(Math.round(track.stats.duration / 1000));
    }

    return this.#authenticatedCall("track.updateNowPlaying", params);
  }

  /**
   * @param {Track} track
   * @param {number} startedAt Unix timestamp in milliseconds
   */
  async scrobble(track, startedAt) {
    const tags = track.tags ?? {};
    /** @type {Record<string, string>} */
    const params = {
      timestamp: String(Math.floor(startedAt / 1000)),
    };

    if (tags.title) params.track = tags.title;
    if (tags.artist) params.artist = tags.artist;
    if (tags.album) params.album = tags.album;
    if (tags.albumartist) params.albumArtist = tags.albumartist;
    if (tags.track?.no != null) params.trackNumber = String(tags.track.no);
    if (track.stats?.duration != null) {
      params.duration = String(Math.round(track.stats.duration / 1000));
    }

    return this.#authenticatedCall("track.scrobble", params);
  }

  // API

  /**
   * @param {Record<string, string>} params
   * @returns {string} MD5 hex digest
   */
  #sign(params) {
    const str = Object.keys(params)
      .sort()
      .map((k) => k + params[k])
      .join("");
    return bytesToHex(md5(utf8ToBytes(str + this.#apiSecret)));
  }

  /**
   * @param {string} method
   * @param {Record<string, string>} [params]
   * @returns {Promise<any>}
   */
  async #call(method, params = {}) {
    const allParams = { ...params, api_key: this.#apiKey, method };
    const api_sig = this.#sign(allParams);
    const body = new URLSearchParams({ ...allParams, api_sig, format: "json" });

    const response = await fetch(ROCKSKY_API_URL, { method: "POST", body });
    const data = await response.json();

    if (data.error) {
      throw new Error(`rocksky error ${data.error}: ${data.message}`);
    }

    return data;
  }

  /**
   * @param {string} method
   * @param {Record<string, string>} [params]
   * @returns {Promise<any>}
   */
  async #authenticatedCall(method, params = {}) {
    const sk = this.#sessionKey.value;
    if (!sk) throw new Error("Not authenticated with Rocksky");
    return this.#call(method, { ...params, sk });
  }
}

export default RockskyScrobbler;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = RockskyScrobbler;
export const NAME = "ds-rocksky-scrobbler";

defineElement(NAME, CLASS);
