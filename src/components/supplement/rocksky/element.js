import { now as tidNow } from "@atcute/tid";

import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { signal } from "~/common/signal.js";
import { clearSession, readSession, saveSession } from "../session.js";

import {
  clearStoredSession,
  DID_STORAGE_KEY,
  getSession,
  login,
  logout,
  OAuthUserAgent,
  restoreOrFinalize,
} from "./oauth.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {ScrobbleElement} from "@specs/components/supplement/types.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

const STORAGE_KEY = "diffuse/supplement/rocksky/session";

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ScrobbleElement}
 */
class RockskyScrobbler extends BroadcastableDiffuseElement {
  static NAME = "diffuse/supplement/rocksky";

  // SIGNALS

  #handle = signal(/** @type {string | null} */ (null));
  #connected = signal(false);
  #isAuthenticating = signal(false);

  // STATE

  handle = this.#handle.get;
  isAuthenticated = this.#connected.get;
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
        setConnected: { strategy: "replicate", fn: this.#connected.set },
      });

      if (actions) {
        this.nowPlaying = actions.nowPlaying;
        this.scrobble = actions.scrobble;

        this.#handle.set = actions.setHandle;
        this.#connected.set = actions.setConnected;
      }
    }

    super.connectedCallback();

    this.#tryRestore();
  }

  async #tryRestore() {
    await this.whenConnected();

    try {
      const session = await restoreOrFinalize();

      if (session) {
        const did = session.info.sub;

        if (await this.isLeader()) {
          this.#connected.set(true);
          this.#handle.set(did);
        } else {
          this.#connected.value = true;
          this.#handle.value = did;
        }

        await saveSession(this, STORAGE_KEY, JSON.stringify({ did }));
        return;
      }
    } catch (err) {
      console.warn("Rocksky: Failed to restore/finalize session", err);
    }

    // Restore previously stored connection state.
    const stored = await readSession(this, STORAGE_KEY);

    if (stored) {
      try {
        const { did } = JSON.parse(stored);

        if (await this.isLeader()) {
          this.#connected.set(true);
          this.#handle.set(did);
        } else {
          this.#connected.value = true;
          this.#handle.value = did;
        }
      } catch {
        await clearSession(this, STORAGE_KEY);
      }
    }
  }

  // AUTH

  /**
   * Connect to Rocksky by initiating the AT Protocol OAuth flow for the given handle.
   * Navigates the browser away to the authorization server.
   *
   * @param {string} handle
   */
  async signIn(handle) {
    this.#isAuthenticating.set(true);

    try {
      await login(handle);
    } finally {
      this.#isAuthenticating.set(false);
    }
  }

  /**
   * Disconnect from Rocksky.
   */
  async signOut() {
    const did = localStorage.getItem(DID_STORAGE_KEY);

    if (did) {
      getSession(/** @type {`did:${string}:${string}`} */ (did))
        .then((session) => logout(new OAuthUserAgent(session)))
        .catch(() => clearStoredSession());
    } else {
      clearStoredSession();
    }

    this.#connected.set(false);
    this.#handle.set(null);
    await clearSession(this, STORAGE_KEY);
  }

  // SCROBBLE ACTIONS

  /**
   * @param {Track} _track
   */
  // deno-lint-ignore no-unused-vars
  async nowPlaying(_track) {
    // Rocksky has no now-playing PDS record type; scrobbles are the source of truth.
  }

  /**
   * @param {Track} track
   * @param {number} startedAt Unix timestamp in milliseconds
   */
  async scrobble(track, startedAt) {
    if (!this.#connected.value) return;

    const did = localStorage.getItem(DID_STORAGE_KEY);
    if (!did) return;

    const session = await getSession(/** @type {`did:${string}:${string}`} */ (did));
    const agent = new OAuthUserAgent(session);

    const tags = track.tags ?? {};

    // All five fields are required by the app.rocksky.scrobble lexicon.
    if (
      !tags.title ||
      !tags.artist ||
      !tags.album ||
      !tags.albumartist ||
      track.stats?.duration == null
    ) return;

    /** @type {Record<string, unknown>} */
    const record = {
      $type: "app.rocksky.scrobble",
      title: tags.title,
      artist: tags.artist,
      album: tags.album,
      albumArtist: tags.albumartist,
      duration: track.stats.duration,
    };

    if (tags.track?.no != null) record.trackNumber = tags.track.no;
    if (tags.disc?.no != null) record.discNumber = tags.disc.no;

    record.createdAt = new Date(startedAt).toISOString();

    const response = await agent.handle("/xrpc/com.atproto.repo.putRecord", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        repo: did,
        collection: "app.rocksky.scrobble",
        rkey: tidNow(),
        record,
        validate: false,
      }),
    });

    if (!response.ok) {
      const error = await response.json().catch(() => ({}));
      throw new Error(`rocksky: scrobble failed ${response.status}: ${error.message ?? ""}`);
    }
  }
}

export default RockskyScrobbler;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = RockskyScrobbler;
export const NAME = "ds-rocksky-scrobbler";

defineElement(NAME, CLASS);
