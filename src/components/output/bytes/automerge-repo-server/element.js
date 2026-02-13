import * as Automerge from "@automerge/automerge";
import { decodeCBOR, encodeCBOR } from "@char/cbor";
import bs58check from "bs58check";

import { DiffuseElement } from "@common/element.js";
import { outputManager } from "../../common.js";

/**
 * @import { DocumentId, PeerId } from "@automerge/automerge-repo"
 * @import { OutputElement, OutputManager, OutputManagerProperties } from "../../types.d.ts"
 */

/**
 * @typedef {{ collection: Uint8Array }} CollectionDocument
 */

const DOC_IDS_STORAGE_KEY = "diffuse/output/automerge-repo-server/doc-ids";
const PROTOCOL_VERSION = "1";

const COLLECTIONS = /** @type {const} */ ([
  "facets",
  "playlists",
  "themes",
  "tracks",
]);

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * Syncs raw Automerge bytes with an automerge-repo sync server
 * over WebSocket, without depending on automerge-repo as a client.
 *
 * Uses the automerge-repo wire protocol (CBOR-framed messages with
 * join handshake) and the standard Automerge sync algorithm.
 *
 * @implements {OutputElement<Uint8Array>}
 */
class AutomergeRepoServerOutput extends DiffuseElement {
  static NAME = "diffuse/output/bytes/automerge-repo-server";

  /** @type {WebSocket | undefined} */
  #ws;

  /** @type {PeerId} */
  #peerId = /** @type {PeerId} */ (`diffuse-${crypto.randomUUID()}`);

  /** @type {PeerId | undefined} */
  #serverPeerId;

  /** @type {Record<string, Automerge.Doc<CollectionDocument>>} */
  #docs = {};

  /** @type {Record<string, Automerge.SyncState>} */
  #syncStates = {};

  /** @type {Record<string, DocumentId>} */
  #docIds = {};

  #manager;

  constructor() {
    super();

    /** @type {OutputManagerProperties<Uint8Array>} */
    const properties = {
      facets: {
        empty: () => undefined,
        get: async () => this.#getBytes("facets"),
        put: async (data) => this.#putBytes("facets", data),
      },
      init: () => this.whenConnected(),
      playlists: {
        empty: () => undefined,
        get: async () => this.#getBytes("playlists"),
        put: async (data) => this.#putBytes("playlists", data),
      },
      themes: {
        empty: () => undefined,
        get: async () => this.#getBytes("themes"),
        put: async (data) => this.#putBytes("themes", data),
      },
      tracks: {
        empty: () => undefined,
        get: async () => this.#getBytes("tracks"),
        put: async (data) => this.#putBytes("tracks", data),
      },
    };

    this.#manager = outputManager(properties);

    this.facets = this.#manager.facets;
    this.playlists = this.#manager.playlists;
    this.themes = this.#manager.themes;
    this.tracks = this.#manager.tracks;
  }

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();
    this.#loadDocIds();
    this.#ensureDocs();
    this.#connect();
  }

  /**
   * @override
   */
  disconnectedCallback() {
    super.disconnectedCallback();
    this.#ws?.close();
    this.#ws = undefined;
  }

  // DOCUMENT MANAGEMENT

  #loadDocIds() {
    const namespace = this.getAttribute("namespace") ?? "automerge-repo-server";
    const storageKey = `${DOC_IDS_STORAGE_KEY}/${namespace}`;
    const stored = localStorage.getItem(storageKey);

    if (stored) {
      this.#docIds = JSON.parse(stored);
    }

    // Ensure every collection has a document ID
    for (const name of COLLECTIONS) {
      if (!this.#docIds[name]) {
        const bytes = crypto.getRandomValues(new Uint8Array(16));

        // Set UUID v4 version and variant bits
        bytes[6] = (bytes[6] & 0x0f) | 0x40;
        bytes[8] = (bytes[8] & 0x3f) | 0x80;

        const docId = /** @type {DocumentId} */ (bs58check.encode(bytes));
        const url = `automerge:${docId}`;

        this.#docIds[name] = docId;
      }
    }

    localStorage.setItem(storageKey, JSON.stringify(this.#docIds));
  }

  #ensureDocs() {
    for (const name of COLLECTIONS) {
      if (!this.#docs[name]) {
        this.#docs[name] = Automerge.init();
      }
      if (!this.#syncStates[name]) {
        this.#syncStates[name] = Automerge.initSyncState();
      }
    }
  }

  /**
   * @param {string} name
   * @returns {Uint8Array}
   */
  #getBytes(name) {
    const doc = this.#docs[name];
    if (doc) return Automerge.save(doc);
    return new Uint8Array();
  }

  /**
   * @param {string} name
   * @param {Uint8Array} data
   */
  #putBytes(name, data) {
    if (data.byteLength > 0) {
      this.#docs[name] = Automerge.load(data);
    } else {
      this.#docs[name] = Automerge.init();
    }

    this.#syncDoc(name);
  }

  // WEBSOCKET CONNECTION

  #connect() {
    const url = this.getAttribute("url");
    if (!url) return;

    const ws = new WebSocket(url);
    ws.binaryType = "arraybuffer";
    this.#ws = ws;

    ws.addEventListener("open", () => {
      this.#sendJoin();
    });

    ws.addEventListener("message", (event) => {
      const msg = this.#cborDecode(new Uint8Array(event.data));
      this.#handleMessage(msg);
    });

    ws.addEventListener("close", () => {
      this.#serverPeerId = undefined;
      this.#scheduleReconnect();
    });

    ws.addEventListener("error", () => {
      ws.close();
    });
  }

  #scheduleReconnect() {
    if (!this.isConnected) return;
    setTimeout(() => {
      if (this.isConnected) this.#connect();
    }, 5000);
  }

  // PROTOCOL

  #sendJoin() {
    this.#send({
      type: "join",
      senderId: this.#peerId,
      peerMetadata: { storageId: undefined, isEphemeral: true },
      supportedProtocolVersions: [PROTOCOL_VERSION],
    });
  }

  /**
   * @param {any} msg
   */
  #handleMessage(msg) {
    switch (msg.type) {
      case "peer":
        this.#serverPeerId = msg.senderId;
        this.#syncAllDocs();
        break;

      case "sync":
      case "request":
        this.#handleSyncMessage(msg);
        break;

      case "doc-unavailable":
        // Server doesn't have this doc; that's fine, we'll push ours
        break;

      case "error":
        console.error("[automerge-repo-server]", msg.message);
        break;
    }
  }

  /**
   * @param {{ documentId: DocumentId, data: Uint8Array }} msg
   */
  #handleSyncMessage(msg) {
    const name = this.#nameForDocId(msg.documentId);
    if (!name) return;

    const doc = this.#docs[name] ?? Automerge.init();
    const syncState = this.#syncStates[name] ?? Automerge.initSyncState();

    const [newDoc, newSyncState] = Automerge.receiveSyncMessage(
      doc,
      syncState,
      msg.data,
    );

    this.#docs[name] = newDoc;
    this.#syncStates[name] = newSyncState;

    // Update the output manager signal with fresh bytes
    // @ts-ignore Not sure what type to use here
    this.#manager.signals[name].value = Automerge.save(newDoc);

    // Continue the sync round-trip
    this.#syncDoc(name);
  }

  // SYNC

  #syncAllDocs() {
    for (const name of COLLECTIONS) {
      this.#syncDoc(name);
    }
  }

  /**
   * @param {string} name
   */
  #syncDoc(name) {
    if (!this.#serverPeerId) return;

    const doc = this.#docs[name];
    const syncState = this.#syncStates[name] ?? Automerge.initSyncState();

    if (!doc) return;

    const [newSyncState, syncMessage] = Automerge.generateSyncMessage(
      doc,
      syncState,
    );

    this.#syncStates[name] = newSyncState;

    if (syncMessage) {
      this.#send({
        type: "sync",
        senderId: this.#peerId,
        targetId: this.#serverPeerId,
        documentId: this.#docIds[name],
        data: syncMessage,
      });
    }
  }

  // HELPERS

  /**
   * @template [T=unknown]
   * @param {Uint8Array} data
   * @returns {T}
   */
  #cborDecode(data) {
    return /** @type {T} */ (decodeCBOR(data));
  }

  /**
   * @param {unknown} data
   */
  #cborEncode(data) {
    return encodeCBOR(data);
  }

  /**
   * @param {object} msg
   */
  #send(msg) {
    if (this.#ws?.readyState === WebSocket.OPEN) {
      this.#ws.send(this.#cborEncode(msg));
    }
  }

  /**
   * @param {DocumentId} documentId
   * @returns {string | undefined}
   */
  #nameForDocId(documentId) {
    for (const name of COLLECTIONS) {
      if (this.#docIds[name] === documentId) return name;
    }
    return undefined;
  }
}

export default AutomergeRepoServerOutput;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AutomergeRepoServerOutput;
export const NAME = "dob-automerge-repo-server";

customElements.define(NAME, AutomergeRepoServerOutput);
