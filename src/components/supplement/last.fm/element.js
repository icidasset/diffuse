import { md5 } from "@noble/hashes/legacy.js";
import { bytesToHex, utf8ToBytes } from "@noble/hashes/utils.js";

import { BroadcastableDiffuseElement } from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ScrobbleElement} from "../types.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

const LASTFM_API_URL = "https://ws.audioscrobbler.com/2.0/";
const LASTFM_AUTH_URL = "https://www.last.fm/api/auth/";
const STORAGE_KEY = "diffuse/supplement/last.fm/session";

const DEFAULT_API_KEY = "4f0fe85b67baef8bb7d008a8754a95e5";
const DEFAULT_API_SECRET = "0cec3ca0f58e04a5082f1131aba1e0d3";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ScrobbleElement}
 */
class LastFmScrobbler extends BroadcastableDiffuseElement {
  static NAME = "diffuse/supplement/last.fm";

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

    // last.fm appends ?token=TOKEN to the callback URL after authorization.
    const urlParams = new URLSearchParams(location.search);
    const urlToken = urlParams.get("token");

    if (urlToken) {
      urlParams.delete("token");
      const newSearch = urlParams.toString();
      history.replaceState(
        null,
        "",
        location.pathname + (newSearch ? "?" + newSearch : "") + location.hash,
      );

      this.#isAuthenticating.set(true);
      try {
        const session = await this.#getSession(urlToken);
        this.#setSession(session);
      } catch (err) {
        console.warn("last.fm: failed to exchange token for session", err);
      } finally {
        this.#isAuthenticating.set(false);
      }

      return;
    }

    // Restore an existing session from localStorage
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
   * Initiate the last.fm auth flow.
   * Redirects the browser to the authorization page; last.fm appends ?token=TOKEN to the callback.
   */
  signIn() {
    const callbackUrl = location.origin + location.pathname + location.search;
    const authUrl = new URL(LASTFM_AUTH_URL);
    authUrl.searchParams.set("api_key", this.#apiKey);
    authUrl.searchParams.set("cb", callbackUrl);

    // Navigate the top-level frame so last.fm's X-Frame-Options doesn't block loading
    // when this element is used inside an iframe.
    (window.top ?? window).location.assign(authUrl.toString());
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
   * Sign a set of API parameters (excluding `format` and `callback`).
   *
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

    const response = await fetch(LASTFM_API_URL, { method: "POST", body });
    const data = await response.json();

    if (data.error) {
      throw new Error(`last.fm error ${data.error}: ${data.message}`);
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
    if (!sk) throw new Error("Not authenticated with last.fm");
    return this.#call(method, { ...params, sk });
  }

  /**
   * @param {string} token
   * @returns {Promise<{ key: string, name: string }>}
   */
  async #getSession(token) {
    const data = await this.#call("auth.getSession", { token });
    return data.session;
  }
}

export default LastFmScrobbler;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = LastFmScrobbler;
export const NAME = "ds-lastfm-scrobbler";

customElements.define(NAME, CLASS);
