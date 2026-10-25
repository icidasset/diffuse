import { now as tidNow } from "@atcute/tid";

import { BroadcastableDiffuseElement, defineElement } from "~/common/element.js";
import { signal } from "~/common/signal.js";
import { clearSession, readSession, saveSession } from "../session.js";

import {
  clearStoredSession,
  DID_STORAGE_KEY,
  login,
  logout,
  OAuthUserAgent,
  restoreOrFinalize,
  TokenRefreshError,
  withSession,
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

        await this.#setConnection(did);
        await saveSession(this, STORAGE_KEY, JSON.stringify({ did }));
        return;
      }

      // There is no session on this device. Never advertise a connection we
      // can't scrobble with; that mismatch is what made a dead session look
      // connected while every scrobble was silently dropped.
      await this.#setConnection(null);
      return;
    } catch (err) {
      // Transient failure (offline, timeout, auth server hiccup). The session
      // is still stored, so keep the persisted state and retry later.
      console.warn("Rocksky: Failed to restore/finalize session", err);
    }

    // Restore previously stored connection state.
    const stored = await readSession(this, STORAGE_KEY);

    if (stored) {
      try {
        const { did } = JSON.parse(stored);

        if (typeof did !== "string") throw new Error("invalid stored session");

        await this.#setConnection(did);
      } catch {
        await clearSession(this, STORAGE_KEY);
      }
    }
  }

  /**
   * Update the connection state and broadcast it to the other instances.
   *
   * @param {string | null} did
   */
  async #setConnection(did) {
    const connected = did !== null;

    if (await this.isLeader()) {
      this.#connected.set(connected);
      this.#handle.set(did);
    } else {
      this.#connected.value = connected;
      this.#handle.value = did;
    }

    if (!connected) {
      await clearSession(this, STORAGE_KEY);
    }
  }

  /**
   * Drop a session that can no longer be refreshed and surface the
   * disconnected state, so the user can reconnect instead of getting silent
   * failures forever.
   */
  async #handleSessionLost() {
    console.warn("Rocksky: session expired or revoked; sign-in required");

    try {
      await clearStoredSession();
    } catch (err) {
      console.warn("Rocksky: Failed to clear stored session", err);
    }

    this.#connected.set(false);
    this.#handle.set(null);
    await clearSession(this, STORAGE_KEY);
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
    // Reflect the disconnect immediately; revocation is best-effort.
    this.#connected.set(false);
    this.#handle.set(null);
    await clearSession(this, STORAGE_KEY);

    try {
      await logout();
    } catch (err) {
      console.warn("Rocksky: Failed to sign out", err);
    }
  }

  // SCROBBLE ACTIONS

  /**
   * @param {Track} _track
   */
  async nowPlaying(_track) {
    // Rocksky has no now-playing PDS record type; scrobbles are the source of truth.
  }

  /**
   * @param {Track} track
   * @param {number} startedAt Unix timestamp in milliseconds
   * @param {{ duration?: number }} [options] duration in milliseconds
   */
  async scrobble(track, startedAt, { duration: durationMs } = {}) {
    if (!this.#connected.value) return;

    const did = localStorage.getItem(DID_STORAGE_KEY);
    if (!did) return;

    const tags = track.tags ?? {};
    const duration = track.stats?.duration ?? durationMs;

    // duration has no meaningful fallback value; skip rather than create an invalid record
    if (duration == null) return;

    /** @type {Record<string, unknown>} */
    const record = {
      $type: "app.rocksky.scrobble",
      createdAt: new Date(startedAt).toISOString(),
      title: tags.title || "Unknown",
      artist: tags.artist || "Unknown",
      album: tags.album || "Unknown",
      albumArtist: tags.albumartist || tags.artist || "Unknown",
      duration,
    };

    if (tags.track?.no != null) record.trackNumber = tags.track.no;
    if (tags.disc?.no != null) record.discNumber = tags.disc.no;

    /** @type {Response} */
    let response;

    try {
      response = await withSession((session) => {
        const agent = new OAuthUserAgent(session);

        return agent.handle("/xrpc/com.atproto.repo.putRecord", {
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
      });
    } catch (err) {
      if (err instanceof TokenRefreshError) {
        await this.#handleSessionLost();
      }

      throw err;
    }

    if (!response.ok) {
      const error = await response.json().catch(() => ({}));

      // `OAuthUserAgent.handle()` swallows a failed token refresh and returns
      // the original 401, so this means the session is unusable rather than
      // the scrobble itself being invalid.
      if (response.status === 401) {
        await this.#handleSessionLost();
      }

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
