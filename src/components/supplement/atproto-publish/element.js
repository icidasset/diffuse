import { Client, ClientResponseError, ok } from "@atcute/client";
import { encode } from "@atcute/cbor";

import {
  BroadcastableDiffuseElement,
  defineElement,
  queryOptional,
} from "~/common/element.js";
import { computed, signal } from "~/common/signal.js";

import {
  clearStoredSession,
  login as oauthLogin,
  logout as oauthLogout,
  OAuthUserAgent,
  restoreOrFinalize,
  TokenRefreshError,
  withOAuth,
} from "~/components/output/raw/atproto-passkey/oauth.js";

import {
  bytesHash,
  groupPlaylistItems,
  playlistRkey,
  stableHash,
} from "./common.js";

/**
 * @import {Session} from "@atcute/oauth-browser-client"
 * @import {Collaboration, Facet, PlaylistItem} from "~/definitions/types.d.ts"
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import {ATProtoPublishElement} from "@specs/components/supplement/atproto-publish/types.d.ts"
 *
 * @typedef {import("@atcute/atproto").ComAtprotoRepoApplyWrites.$input['writes'][number]} WriteOp
 * @typedef {{ hash: string, rkey: string, playlist?: string }} PublishedRecord
 * @typedef {{
 *   collaborations: Record<string, PublishedRecord>,
 *   playlists: Record<string, PublishedRecord>,
 *   facets: Record<string, PublishedRecord>,
 * }} PublishState
 * @typedef {{ rkey: string, playlist: string, bytes: Uint8Array, hash: string }} PlaylistUpsert
 * @typedef {{ id: string, record: Record<string, unknown>, hash: string }} RecordUpsert
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

const COLLABORATION = "sh.diffuse.output.collaboration";
const FACET = "sh.diffuse.output.facet";
const PLAYLIST_ITEM_BUNDLE = "sh.diffuse.output.playlistItemBundle";

/** Local record of what this element has already published, per account. */
const STORAGE_PREFIX = "diffuse/supplement/atproto-publish";

/** Debounce for reacting to output changes (collection signals fire rapidly). */
const SYNC_DEBOUNCE_MS = 1500;

/** Maximum number of deletes per `applyWrites` call. */
const DELETE_BATCH_SIZE = 100;

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Publishes playlist items, facets and collaborations to the user's AT
 * Protocol account.
 *
 * This is deliberately not an output: it reads from whichever output is
 * selected and publishes a copy to the account connected through the public
 * AT Protocol output's OAuth session. That makes it usable alongside any
 * storage method (S3, Dropbox, the local output, …).
 *
 * @implements {ATProtoPublishElement}
 */
class ATProtoPublish extends BroadcastableDiffuseElement {
  static NAME = "diffuse/supplement/atproto-publish";
  static observedAttributes = ["output-selector"];

  // SIGNALS

  #did = signal(/** @type {`did:${string}:${string}` | null} */ (null));
  #handle = signal(/** @type {string | null} */ (null));
  #isOnline = signal(
    typeof navigator === "undefined" ? true : navigator.onLine,
  );
  #authenticated = signal(false);
  #syncing = signal(false);
  #error = signal(/** @type {string | null} */ (null));
  #lastSyncedAt = signal(/** @type {number | null} */ (null));
  #output = signal(/** @type {OutputElement<any> | null} */ (null));

  /** @type {PromiseWithResolvers<void>} */
  #restoreSettled = Promise.withResolvers();

  did = this.#did.get;
  handle = this.#handle.get;
  syncing = this.#syncing.get;
  error = this.#error.get;
  lastSyncedAt = this.#lastSyncedAt.get;

  ready = computed(() => {
    return this.#authenticated.value && this.#did.value !== null &&
      this.#isOnline.value;
  });

  // SESSION

  /** @type {OAuthUserAgent | null} */
  #agent = null;

  /** @type {Client | null} */
  #rpc = null;

  /** Whether a session restore is currently in flight. */
  #restoring = false;

  // SYNC

  /** @type {ReturnType<typeof setTimeout> | null} */
  #timer = null;
  #running = false;
  #queued = false;

  // LIFECYCLE

  /** @override */
  connectedCallback() {
    // Only the leader writes, so two tabs can't upload or delete the same
    // record at the same time.
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(this.identifier, {
        publish: { strategy: "leaderOnly", fn: this.publish },
      });

      if (actions) this.publish = actions.publish;
    }

    super.connectedCallback();

    this.#resolveOutput();
    this.#installWatch();
    this.#tryRestore();

    globalThis.addEventListener("online", this.#online);
    globalThis.addEventListener("offline", this.#offline);
  }

  /** @override */
  disconnectedCallback() {
    globalThis.removeEventListener("online", this.#online);
    globalThis.removeEventListener("offline", this.#offline);

    if (this.#timer !== null) {
      clearTimeout(this.#timer);
      this.#timer = null;
    }

    super.disconnectedCallback();
  }

  /**
   * @override
   * @param {string} name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);

    if (
      name === "output-selector" && this.isConnected && oldValue !== newValue
    ) {
      this.#resolveOutput();
    }
  }

  #offline = () => this.#isOnline.set(false);

  #online = () => {
    this.#isOnline.set(true);

    // A restore may have failed while offline; give it another chance instead
    // of staying silently disconnected until the page is reloaded.
    if (!this.#rpc) this.#tryRestore();
    else this.#schedule();
  };

  // AUTH

  /**
   * Initiate the OAuth flow. This is the same session as the public AT
   * Protocol output, so signing in here also signs that output in.
   *
   * @param {string} handle
   */
  async login(handle) {
    await oauthLogin(handle);
  }

  /**
   * Sign out and revoke the shared AT Protocol session.
   */
  async logout() {
    const agent = this.#agent;
    this.#resetSession();
    if (agent) await oauthLogout(agent);
  }

  /**
   * @returns {Promise<void>}
   */
  whenRestored() {
    return this.#restoreSettled.promise;
  }

  async #tryRestore() {
    if (this.#restoring) return;
    this.#restoring = true;

    try {
      await this.whenConnected();

      try {
        const session = await restoreOrFinalize();

        if (session) {
          this.#setSession(session);
        } else if (this.#rpc) {
          this.#resetSession();
        } else {
          console.info(
            "ATProto publish: no AT Protocol session; connect Atmosphere (Public) to publish.",
          );
        }
      } catch (err) {
        if (this.#isSessionError(err)) {
          this.#resetSession();
        } else {
          // Transient failure (offline, timeout, auth server hiccup). The
          // stored session is intact; retry on the next online event.
          console.warn("ATProto publish: failed to restore the session", err);
        }
      }
    } finally {
      this.#restoring = false;
      this.#restoreSettled.resolve();
    }
  }

  /**
   * @param {Session} session
   */
  #setSession(session) {
    const agent = new OAuthUserAgent(session);

    // All RPC goes through `agent.handle()`. Run it with the atcute library
    // configured for the public AT Protocol output under the shared OAuth
    // lock; otherwise another module (Rocksky) can leave the globals pointing
    // at its own database/client id and a token refresh here would fail.
    const originalHandle = agent.handle.bind(agent);
    agent.handle = /** @type {typeof originalHandle} */ ((pathname, init) => {
      return withOAuth(() => originalHandle(pathname, init));
    });

    this.#agent = agent;
    this.#rpc = new Client({ handler: agent });
    this.#did.value = session.info.sub;
    this.#authenticated.value = true;
    this.#error.value = null;
    this.#fetchHandle(session.info.sub);
  }

  #resetSession() {
    this.#agent = null;
    this.#rpc = null;
    this.#did.value = null;
    this.#handle.value = null;
    this.#authenticated.value = false;
  }

  /**
   * Drop a session that can no longer be refreshed so future syncs stop
   * silently failing.
   */
  #handleSessionLost() {
    this.#resetSession();
    this.#error.value = "The AT Protocol session is no longer valid.";

    // Don't await: a failed refresh may still hold the OAuth lock. The queued
    // clear runs afterwards.
    clearStoredSession().catch((err) => {
      console.warn("ATProto publish: failed to clear the stored session", err);
    });
  }

  /**
   * @param {unknown} err
   * @returns {boolean}
   */
  #isSessionError(err) {
    if (err instanceof TokenRefreshError) return true;
    // OAuthUserAgent.handle() swallows TokenRefreshError and returns the
    // original 401 response, which ok() wraps as a ClientResponseError.
    if (err instanceof ClientResponseError && err.status === 401) return true;
    if (err && typeof err === "object" && "cause" in err) {
      return this.#isSessionError(
        (/** @type {{ cause: unknown }} */ (err)).cause,
      );
    }
    return false;
  }

  /**
   * @param {string} did
   */
  async #fetchHandle(did) {
    const rpc = this.#rpc;
    if (!rpc) return;

    try {
      const result = await ok(rpc.get("com.atproto.repo.describeRepo", {
        params: {
          repo: /** @type {import("@atcute/lexicons").ActorIdentifier} */ (did),
        },
      }));

      if (this.#did.value === did) {
        this.#handle.value = result?.handle ?? null;
      }
    } catch {
      // Non-fatal; handle stays null
    }
  }

  // OUTPUT

  async #resolveOutput() {
    const output = /** @type {Element | null} */ (
      queryOptional(this, "output-selector") ??
        document.querySelector("do-output")
    );

    if (!output) return;

    await customElements.whenDefined(output.localName);

    if (!(await this.#waitForCollections(output))) return;

    this.#output.value = /** @type {OutputElement<any>} */ (
      /** @type {unknown} */ (output)
    );
  }

  /**
   * The orchestrator's proxied collections are only reachable once it has
   * rendered its output chain. Poll briefly instead of reading a getter that
   * throws.
   *
   * @param {Element} output
   * @returns {Promise<boolean>}
   */
  async #waitForCollections(output) {
    for (let attempt = 0; attempt < 100; attempt++) {
      try {
        const out = /** @type {OutputElement<any>} */ (
          /** @type {unknown} */ (output)
        );
        out.playlistItems.collection();
        out.facets.collection();
        out.collaborations.collection();
        return true;
      } catch {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }
    }

    console.warn("ATProto publish: the output never became available");
    return false;
  }

  /**
   * Publish whenever the selected output's collections change.
   */
  #installWatch() {
    this.effect(() => {
      const output = this.#output.value;
      if (!output) return;
      if (!this.#authenticated.value) return;

      try {
        const playlistItems = output.playlistItems.collection();
        const facets = output.facets.collection();
        const collaborations = output.collaborations.collection();

        if (playlistItems.state !== "loaded") return;
        if (facets.state !== "loaded") return;
        if (collaborations.state !== "loaded") return;

        // Read the data so changes are tracked by the effect.
        void playlistItems.data;
        void facets.data;
        void collaborations.data;
      } catch {
        return;
      }

      if (this.#isOnline.value) this.#schedule();
    });
  }

  // SYNC

  #schedule() {
    if (!this.ready()) return;

    if (this.#timer !== null) clearTimeout(this.#timer);

    this.#timer = setTimeout(() => {
      this.#timer = null;
      void this.publish();
    }, SYNC_DEBOUNCE_MS);
  }

  /**
   * Publish the selected output's playlist items, facets and collaborations
   * now.
   *
   * @returns {Promise<void>}
   */
  async publish() {
    if (!this.ready()) return;

    if (this.#running) {
      this.#queued = true;
      return;
    }

    this.#running = true;
    this.#syncing.value = true;
    this.#error.value = null;

    try {
      await this.#publishNow();
      this.#lastSyncedAt.value = Date.now();
    } catch (err) {
      if (this.#isSessionError(err)) {
        this.#handleSessionLost();
      } else {
        console.warn("ATProto publish: could not publish", err);
        this.#error.value = err instanceof Error ? err.message : String(err);
      }
    } finally {
      this.#syncing.value = false;
      this.#running = false;

      if (this.#queued) {
        this.#queued = false;
        this.#schedule();
      }
    }
  }

  async #publishNow() {
    const output = this.#output.value;
    const did = this.#did.value;
    if (!output || !this.#rpc || !did) return;

    // The public output already stores these records in the account; writing
    // them again would only burn rate limit without changing anything.
    if (this.#alreadyInPublicRepo(output)) return;

    const playlistCollection = output.playlistItems.collection();
    const facetCollection = output.facets.collection();
    const collaborationCollection = output.collaborations.collection();

    if (playlistCollection.state !== "loaded") return;
    if (facetCollection.state !== "loaded") return;
    if (collaborationCollection.state !== "loaded") return;

    const playlistItems = /** @type {PlaylistItem[]} */ (
      playlistCollection.data ?? []
    );
    const facets = /** @type {Facet[]} */ (facetCollection.data ?? []);
    const collaborations = /** @type {Collaboration[]} */ (
      collaborationCollection.data ?? []
    );

    const groups = groupPlaylistItems(playlistItems);
    const previous = this.#readState(did);

    /** @type {PlaylistUpsert[]} */
    const playlistUpserts = [];
    /** @type {Record<string, PublishedRecord>} */
    const nextPlaylists = { ...previous.playlists };

    for (const [playlist, items] of groups) {
      const bytes = encode(items);
      const hash = bytesHash(bytes);
      const rkey = playlistRkey(playlist);

      if (previous.playlists[rkey]?.hash === hash) continue;

      playlistUpserts.push({ rkey, playlist, bytes, hash });
    }

    const desiredPlaylistKeys = new Set(
      [...groups.keys()].map((playlist) => playlistRkey(playlist)),
    );
    const playlistDeletes = Object.keys(previous.playlists).filter(
      (rkey) => !desiredPlaylistKeys.has(rkey),
    );

    /** @type {Map<string, Record<string, unknown>>} */
    const desiredCollaborations = new Map();

    for (const collaboration of collaborations) {
      if (!collaboration || typeof collaboration.id !== "string") continue;
      desiredCollaborations.set(collaboration.id, {
        ...collaboration,
        $type: COLLABORATION,
      });
    }

    /** @type {RecordUpsert[]} */
    const collaborationUpserts = [];
    /** @type {Record<string, PublishedRecord>} */
    const nextCollaborations = { ...previous.collaborations };

    for (const [id, record] of desiredCollaborations) {
      const hash = stableHash(record);
      if (previous.collaborations[id]?.hash === hash) continue;
      collaborationUpserts.push({ id, record, hash });
    }

    const collaborationDeletes = Object.keys(previous.collaborations).filter(
      (id) => !desiredCollaborations.has(id),
    );

    /** @type {Map<string, Record<string, unknown>>} */
    const desiredFacets = new Map();

    for (const facet of facets) {
      if (!facet || typeof facet.id !== "string") continue;
      desiredFacets.set(facet.id, { ...facet, $type: FACET });
    }

    /** @type {RecordUpsert[]} */
    const facetUpserts = [];
    /** @type {Record<string, PublishedRecord>} */
    const nextFacets = { ...previous.facets };

    for (const [id, record] of desiredFacets) {
      const hash = stableHash(record);
      if (previous.facets[id]?.hash === hash) continue;
      facetUpserts.push({ id, record, hash });
    }

    const facetDeletes = Object.keys(previous.facets).filter(
      (id) => !desiredFacets.has(id),
    );

    // Playlists are stored as one bundle record (with a CBOR blob) per
    // playlist, matching the public output's data shape.
    for (const { rkey, playlist, bytes, hash } of playlistUpserts) {
      const blob = await this.#uploadBlob(bytes);
      if (!blob) continue;

      await this.#putRecord(PLAYLIST_ITEM_BUNDLE, rkey, {
        $type: PLAYLIST_ITEM_BUNDLE,
        id: rkey,
        playlist,
        data: blob,
      });

      nextPlaylists[rkey] = { hash, rkey, playlist };
    }

    for (const { id, record, hash } of collaborationUpserts) {
      await this.#putRecord(COLLABORATION, id, record);
      nextCollaborations[id] = { hash, rkey: id };
    }

    for (const { id, record, hash } of facetUpserts) {
      await this.#putRecord(FACET, id, record);
      nextFacets[id] = { hash, rkey: id };
    }

    // Only delete records this element published itself. Another client (or
    // the public output) may store other records in these collections that the
    // selected output doesn't know about; those must be left alone.
    if (playlistDeletes.length > 0) {
      await this.#applyDeletes(PLAYLIST_ITEM_BUNDLE, playlistDeletes);
      for (const rkey of playlistDeletes) delete nextPlaylists[rkey];
    }

    if (facetDeletes.length > 0) {
      await this.#applyDeletes(FACET, facetDeletes);
      for (const id of facetDeletes) delete nextFacets[id];
    }

    if (collaborationDeletes.length > 0) {
      await this.#applyDeletes(COLLABORATION, collaborationDeletes);
      for (const id of collaborationDeletes) delete nextCollaborations[id];
    }

    this.#writeState(did, {
      collaborations: nextCollaborations,
      playlists: nextPlaylists,
      facets: nextFacets,
    });
  }

  /**
   * Whether the selected output already stores the data in the public AT
   * Protocol repo. The space output is not skipped: its data lives in a space,
   * so publishing a public copy is still the point of this element.
   *
   * @param {OutputElement<any>} output
   * @returns {boolean}
   */
  #alreadyInPublicRepo(output) {
    const configurable = /** @type {any} */ (output);

    if (typeof configurable.selected !== "function") return false;

    const selected = configurable.selected();
    if (!selected) return false;

    const connect = selected.getAttribute?.("connect") ?? "";
    return connect.includes("atproto-passkey");
  }

  // RPC

  /**
   * @param {string} collection
   * @param {string} rkey
   * @param {Record<string, unknown>} record
   */
  async #putRecord(collection, rkey, record) {
    const rpc = this.#rpc;
    const did = this.#did.value;
    if (!rpc || !did) return;

    await ok(rpc.post("com.atproto.repo.putRecord", {
      input: {
        repo: did,
        collection: /** @type {`${string}.${string}.${string}`} */ (collection),
        rkey,
        record,
      },
    }));
  }

  /**
   * @param {string} collection
   * @param {string[]} rkeys
   */
  async #applyDeletes(collection, rkeys) {
    const rpc = this.#rpc;
    const did = this.#did.value;
    if (!rpc || !did) return;

    for (let i = 0; i < rkeys.length; i += DELETE_BATCH_SIZE) {
      /** @type {WriteOp[]} */
      const writes = rkeys.slice(i, i + DELETE_BATCH_SIZE).map((rkey) => ({
        $type: "com.atproto.repo.applyWrites#delete",
        collection: /** @type {`${string}.${string}.${string}`} */ (collection),
        rkey,
      }));

      await ok(rpc.post("com.atproto.repo.applyWrites", {
        input: { repo: did, writes },
      }));
    }
  }

  /**
   * @param {Uint8Array} bytes
   */
  async #uploadBlob(bytes) {
    const rpc = this.#rpc;
    if (!rpc) return;

    const result = await ok(rpc.post("com.atproto.repo.uploadBlob", {
      input: bytes,
      headers: { "content-type": "application/octet-stream" },
    }));

    return result?.blob;
  }

  // STATE

  /**
   * @param {string} did
   * @returns {PublishState}
   */
  #readState(did) {
    try {
      const raw = localStorage.getItem(`${STORAGE_PREFIX}/${did}`);
      const parsed = raw ? JSON.parse(raw) : null;

      if (parsed && typeof parsed === "object") {
        return {
          collaborations:
            parsed.collaborations && typeof parsed.collaborations === "object"
              ? parsed.collaborations
              : {},
          playlists: parsed.playlists && typeof parsed.playlists === "object"
            ? parsed.playlists
            : {},
          facets: parsed.facets && typeof parsed.facets === "object"
            ? parsed.facets
            : {},
        };
      }
    } catch {
      // A corrupt cache just means everything is published again.
    }

    return { collaborations: {}, playlists: {}, facets: {} };
  }

  /**
   * @param {string} did
   * @param {PublishState} state
   */
  #writeState(did, state) {
    try {
      localStorage.setItem(
        `${STORAGE_PREFIX}/${did}`,
        JSON.stringify(state),
      );
    } catch (err) {
      console.warn(
        "ATProto publish: could not persist the published state",
        err,
      );
    }
  }
}

export default ATProtoPublish;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = ATProtoPublish;
export const NAME = "ds-atproto-publish";

defineElement(NAME, CLASS);
