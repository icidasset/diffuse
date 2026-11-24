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

const STORAGE_KEY = "diffuse/supplement/teal.fm/session";

const PLAY_COLLECTION = "fm.teal.feed.play";
const STATUS_COLLECTION = "fm.teal.actor.status";
const STATUS_RKEY = "self";

const SUBMISSION_CLIENT_AGENT = "sh.diffuse/unknown";

/**
 * How long a now-playing status stays current when the track length is unknown.
 * Matches the teal.fm lexicon's documented default.
 */
const DEFAULT_STATUS_EXPIRY_MS = 10 * 60 * 1000;

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {ScrobbleElement}
 */
class TealScrobbler extends BroadcastableDiffuseElement {
  static NAME = "diffuse/supplement/teal.fm";

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
      console.warn("teal.fm: Failed to restore/finalize session", err);
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
    console.warn("teal.fm: session expired or revoked; sign-in required");

    try {
      await clearStoredSession();
    } catch (err) {
      console.warn("teal.fm: Failed to clear stored session", err);
    }

    this.#connected.set(false);
    this.#handle.set(null);
    await clearSession(this, STORAGE_KEY);
  }

  // AUTH

  /**
   * Connect to teal.fm by initiating the AT Protocol OAuth flow for the given
   * handle. Navigates the browser away to the authorization server.
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
   * Disconnect from teal.fm.
   */
  async signOut() {
    // Reflect the disconnect immediately; revocation is best-effort.
    this.#connected.set(false);
    this.#handle.set(null);
    await clearSession(this, STORAGE_KEY);

    try {
      await logout();
    } catch (err) {
      console.warn("teal.fm: Failed to sign out", err);
    }
  }

  // SCROBBLE ACTIONS

  /**
   * @param {Track} track
   */
  async nowPlaying(track) {
    if (!this.#connected.value) return;

    const did = localStorage.getItem(DID_STORAGE_KEY);
    if (!did) return;

    const startedAt = Date.now();
    const durationMs = track.stats?.duration;
    const expiryMs = durationMs != null && durationMs > 0
      ? durationMs
      : DEFAULT_STATUS_EXPIRY_MS;

    await this.#putRecord(did, STATUS_COLLECTION, STATUS_RKEY, {
      time: new Date(startedAt).toISOString(),
      expiry: new Date(startedAt + expiryMs).toISOString(),
      item: this.#playItem(track, startedAt, durationMs),
    });
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

    await this.#putRecord(
      did,
      PLAY_COLLECTION,
      tidNow(),
      this.#playItem(track, startedAt, durationMs),
    );
  }

  // RECORDS

  /**
   * Build a `fm.teal.feed.defs#playView` from a track.
   *
   * @param {Track} track
   * @param {number} playedAt Unix timestamp in milliseconds
   * @param {number | undefined} durationMs
   * @returns {Record<string, unknown>}
   */
  #playItem(track, playedAt, durationMs) {
    const tags = track.tags ?? {};

    const artistNames = (tags.artists?.length ? tags.artists : [tags.artist])
      .map((name) => name?.trim())
      .filter((name) => !!name);

    /** @type {Record<string, unknown>} */
    const item = {
      trackName: tags.title?.trim() || "Unknown",
      artists: (artistNames.length ? artistNames : ["Unknown"]).map(
        (artistName) => ({ artistName }),
      ),
      playedTime: new Date(playedAt).toISOString(),
      submissionClientAgent: SUBMISSION_CLIENT_AGENT,
    };

    const duration = track.stats?.duration ?? durationMs;
    if (duration != null && duration > 0) {
      // teal.fm records the duration in whole seconds.
      item.duration = Math.max(1, Math.round(duration / 1000));
    }

    const releaseName = tags.album?.trim();
    if (releaseName) item.releaseName = releaseName;

    const isrc = tags.isrc?.find((code) => !!code);
    if (isrc) item.isrc = isrc;

    // `originUri` expects an absolute URI; a local path isn't one, so skip it.
    const originUri = track.uri?.trim();
    if (originUri && /^[a-z][a-z0-9+.-]*:/i.test(originUri)) {
      item.originUri = originUri;
    }

    return item;
  }

  /**
   * Write a record to the signed-in user's repository.
   *
   * @param {string} did
   * @param {string} collection
   * @param {string} rkey
   * @param {Record<string, unknown>} record
   */
  async #putRecord(did, collection, rkey, record) {
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
            collection,
            rkey,
            record: { $type: collection, ...record },
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
      // the record itself being invalid.
      if (response.status === 401) {
        await this.#handleSessionLost();
      }

      throw new Error(
        `teal.fm: ${collection} write failed ${response.status}: ${
          error.message ?? ""
        }`,
      );
    }
  }
}

export default TealScrobbler;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = TealScrobbler;
export const NAME = "ds-teal-scrobbler";

defineElement(NAME, CLASS);
