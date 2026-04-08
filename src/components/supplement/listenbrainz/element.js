import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";
import { clearSession, readSession, saveSession } from "../session.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ScrobbleElement} from "../types.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

const API_BASE = "https://api.listenbrainz.org/1/";
const STORAGE_KEY = "diffuse/supplement/listenbrainz/session";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ScrobbleElement}
 */
class ListenBrainzScrobbler extends BroadcastableDiffuseElement {
  static NAME = "diffuse/supplement/listenbrainz";

  // SIGNALS

  #handle = signal(/** @type {string | null} */ (null));
  #userToken = signal(/** @type {string | null} */ (null));
  #isAuthenticating = signal(false);

  // STATE

  handle = this.#handle.get;
  isAuthenticated = computed(() => this.#userToken.value !== null);
  isAuthenticating = this.#isAuthenticating.get;

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        nowPlaying: { strategy: "leaderOnly", fn: this.nowPlaying },
        scrobble: { strategy: "leaderOnly", fn: this.scrobble },

        setHandle: { strategy: "replicate", fn: this.#handle.set },
        setUserToken: { strategy: "replicate", fn: this.#userToken.set },
      });

      if (actions) {
        this.nowPlaying = actions.nowPlaying;
        this.scrobble = actions.scrobble;

        this.#handle.set = actions.setHandle;
        this.#userToken.set = actions.setUserToken;
      }
    }

    super.connectedCallback();

    this.#tryRestore();
  }

  async #tryRestore() {
    await this.whenConnected();

    const stored = await readSession(this, STORAGE_KEY);

    if (stored) {
      try {
        const { token, username } = JSON.parse(stored);

        if (await this.isLeader()) {
          this.#userToken.set(token);
          this.#handle.set(username);
        } else {
          this.#userToken.value = token;
          this.#handle.value = username;
        }
      } catch {
        await clearSession(this, STORAGE_KEY);
      }
    }
  }

  // AUTH

  /**
   * Validate a ListenBrainz user token and store the session.
   *
   * @param {string} token
   */
  async signIn(token) {
    this.#isAuthenticating.set(true);
    try {
      const username = await this.#validateToken(token);
      this.#userToken.set(token);
      this.#handle.set(username);
      await saveSession(this, STORAGE_KEY, JSON.stringify({ token, username }));
    } catch (err) {
      console.warn("listenbrainz: failed to authenticate", err);
      throw err;
    } finally {
      this.#isAuthenticating.set(false);
    }
  }

  /**
   * Clear the stored session.
   */
  async signOut() {
    this.#userToken.set(null);
    this.#handle.set(null);
    await clearSession(this, STORAGE_KEY);
  }

  // SCROBBLE ACTIONS

  /**
   * @param {Track} track
   */
  async nowPlaying(track) {
    return this.#submit("playing_now", [
      { track_metadata: this.#trackMetadata(track) },
    ]);
  }

  /**
   * @param {Track} track
   * @param {number} startedAt Unix timestamp in milliseconds
   */
  async scrobble(track, startedAt) {
    return this.#submit("single", [
      {
        listened_at: Math.floor(startedAt / 1000),
        track_metadata: this.#trackMetadata(track),
      },
    ]);
  }

  // API

  /**
   * @param {Track} track
   * @returns {Record<string, unknown>}
   */
  #trackMetadata(track) {
    const tags = track.tags ?? {};
    /** @type {Record<string, unknown>} */
    const additional_info = { submission_client: "Diffuse" };

    if (track.stats?.duration != null) {
      additional_info.duration_ms = track.stats.duration;
    }
    if (tags.track?.no != null) {
      additional_info.tracknumber = tags.track.no;
    }

    /** @type {Record<string, unknown>} */
    const metadata = { additional_info };

    if (tags.title) metadata.track_name = tags.title;
    if (tags.artist) metadata.artist_name = tags.artist;
    if (tags.album) metadata.release_name = tags.album;

    return metadata;
  }

  /**
   * @param {string} token
   * @returns {Promise<string>} username
   */
  async #validateToken(token) {
    const response = await fetch(`${API_BASE}validate-token`, {
      headers: { Authorization: `Token ${token}` },
    });
    const data = await response.json();

    if (!data.valid) {
      throw new Error(
        `listenbrainz: invalid token — ${data.message ?? "unknown error"}`,
      );
    }

    return data.user_name;
  }

  /**
   * @param {string} listen_type
   * @param {unknown[]} payload
   * @returns {Promise<any>}
   */
  async #submit(listen_type, payload) {
    const token = this.#userToken.value;
    if (!token) throw new Error("Not authenticated with ListenBrainz");

    const response = await fetch(`${API_BASE}submit-listens`, {
      method: "POST",
      headers: {
        Authorization: `Token ${token}`,
        "Content-Type": "application/json",
      },
      body: JSON.stringify({ listen_type, payload }),
    });

    const data = await response.json();

    if (data.code && data.code !== 200) {
      throw new Error(`listenbrainz error ${data.code}: ${data.error}`);
    }

    return data;
  }
}

export default ListenBrainzScrobbler;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ListenBrainzScrobbler;
export const NAME = "ds-listenbrainz-scrobbler";

defineElement(NAME, CLASS);
