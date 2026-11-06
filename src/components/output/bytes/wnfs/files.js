import { RpcChannel } from "~/common/worker/rpc-channel.js";

/**
 * Browser-side access to the audio files referenced by the Firkin + WNFS.
 *
 * Audio bytes live in Firkin as raw blobs (content-addressed). The WNFS only
 * stores a small pointer map: `<name> → { cid, size, mime, modified }`. Input
 * and upload workers talk to the output element over a `BroadcastChannel`; the
 * output owns the WNFS mount and the Firkin session.
 */

/** URI scheme for files in the Firkin + WNFS WNFS. */
export const SCHEME = "wnfs+firkin";

/** The single URI representing the current Firkin + WNFS account. */
export const SOURCE_URI = `${SCHEME}://me`;

/** Private WNFS file holding the `name → Firkin CID` pointer map. */
export const FILES_MANIFEST = "diffuse-files.json";

/** Path segment used in file URIs (user-facing). */
export const URI_DIR = "files";

/** Identifier prefix of the output element that owns the WNFS. */
export const OUTPUT_NAME = "diffuse/output/bytes/wnfs";

/**
 * How long a presigned Firkin read URL stays valid.
 *
 * The queue-audio orchestrator re-resolves and reloads a URL shortly before
 * it lapses, so this is a ceiling rather than the expected link lifetime —
 * a leaked URL stays a usable bearer capability for at most this long.
 */
export const FILE_URL_TTL_SECONDS = 6 * 60 * 60;

/**
 * @typedef {Object} StorageFile
 * @property {string} name
 * @property {string} cid - Raw Firkin blob CID.
 * @property {number} size - Bytes.
 * @property {string} mime
 * @property {number} modified - Unix epoch milliseconds.
 */

/**
 * @typedef {Object} FilesManifest
 * @property {number} version
 * @property {Record<string, Omit<StorageFile, "name">>} files
 */

/**
 * @param {Uint8Array} bytes
 * @returns {FilesManifest}
 */
export function decodeManifest(bytes) {
  try {
    const parsed = JSON.parse(new TextDecoder().decode(bytes));
    if (
      parsed && typeof parsed === "object" && parsed.files &&
      typeof parsed.files === "object"
    ) {
      return { version: 1, files: parsed.files };
    }
  } catch (err) {
    console.error("Failed to decode the Firkin + WNFS file manifest:", err);
  }

  return { version: 1, files: {} };
}

/**
 * @param {FilesManifest} manifest
 * @returns {Uint8Array}
 */
export function encodeManifest(manifest) {
  return new TextEncoder().encode(JSON.stringify(manifest));
}

/**
 * @typedef {Object} StorageStatus
 * @property {boolean} connected
 * @property {string} [did]
 * @property {string} [root]
 */

/**
 * `BroadcastChannel` name of the output element for the given group and
 * namespace. Mirrors `DiffuseElement#identifier` for the output element.
 *
 * @param {{ group?: string; namespace?: string }} options
 */
export function outputChannelName({ group, namespace }) {
  return `${OUTPUT_NAME}/${group || "default"}${
    namespace ? `/${namespace}` : ""
  }`;
}

/**
 * URI of a file inside the Firkin + WNFS.
 *
 * @param {string} name
 */
export function buildURI(name) {
  return `${SOURCE_URI}/${URI_DIR}/${encodeURIComponent(name)}`;
}

/**
 * @param {string} uri
 * @returns {{ name: string } | null}
 */
export function parseURI(uri) {
  const prefix = `${SOURCE_URI}/`;
  if (!uri.startsWith(prefix)) return null;

  const segments = uri.slice(prefix.length).split("/");
  if (segments[0] !== URI_DIR || segments.length !== 2) return null;

  try {
    const name = decodeURIComponent(segments[1]);
    return name.length > 0 ? { name } : null;
  } catch {
    return null;
  }
}

/** @type {Record<string, string>} */
const MIME_TYPES = {
  aac: "audio/aac",
  aiff: "audio/aiff",
  alac: "audio/mp4",
  flac: "audio/flac",
  m4a: "audio/mp4",
  m4b: "audio/mp4",
  mp3: "audio/mpeg",
  mp4: "audio/mp4",
  oga: "audio/ogg",
  ogg: "audio/ogg",
  opus: "audio/ogg",
  wav: "audio/wav",
  webm: "audio/webm",
  wma: "audio/x-ms-wma",
};

/**
 * @param {string} name
 */
export function mimeType(name) {
  const ext = name.slice(name.lastIndexOf(".") + 1).toLowerCase();
  return MIME_TYPES[ext] ?? "application/octet-stream";
}

/** Per-action timeouts. Writes wait for the debounced `/dag/push`. */
const TIMEOUT = {
  status: 3_000,
  list: 30_000,
  read: 60_000,
  fileURL: 10_000,
  upload: 120_000,
  delete: 120_000,
};

const NOT_REACHABLE =
  "Firkin + WNFS is not reachable. Sign in on the Connect → Firkin + WNFS page first.";

/**
 * RPC client for the output element's WNFS file actions.
 *
 * One instance per worker; the broadcast channel is opened lazily on the first
 * call and reused afterwards.
 */
export class StorageClient {
  /** @type {string} */
  #channelName;

  /** @type {BroadcastChannel | undefined} */
  #channel;

  /** @type {RpcChannel<{}, StorageActions> | undefined} */
  #rpc;

  /**
   * @param {{ group?: string; namespace?: string }} options
   */
  constructor(options) {
    this.#channelName = outputChannelName(options);
  }

  /** @returns {Promise<StorageStatus>} */
  status() {
    return this.#call("storageStatus", [], TIMEOUT.status);
  }

  /** @returns {Promise<StorageFile[]>} */
  listFiles() {
    return this.#call("storageListFiles", [], TIMEOUT.list);
  }

  /**
   * @param {string} name
   * @returns {Promise<Uint8Array>}
   */
  read(name) {
    return this.#call("storageRead", [name], TIMEOUT.read);
  }

  /**
   * A short-lived, presigned Firkin URL for this file.
   *
   * @param {string} name
   * @param {number} [expireIn]
   * @returns {Promise<string | null>}
   */
  fileURL(name, expireIn) {
    return this.#call("storageFileURL", [name, expireIn], TIMEOUT.fileURL);
  }

  /**
   * Upload a file as a raw Firkin blob and record its pointer in the WNFS.
   *
   * @param {string} name
   * @param {Uint8Array} bytes
   * @param {string} mime
   * @returns {Promise<{ cid: string } | null>}
   */
  uploadFile(name, bytes, mime) {
    return this.#call(
      "storageUploadFile",
      [name, bytes, mime],
      TIMEOUT.upload,
    );
  }

  /**
   * @param {string} name
   * @returns {Promise<void>}
   */
  deleteFile(name) {
    return this.#call("storageDeleteFile", [name], TIMEOUT.delete);
  }

  close() {
    this.#channel?.close();
    this.#channel = undefined;
    this.#rpc = undefined;
  }

  /**
   * @param {keyof StorageActions & string} method
   * @param {any[]} args
   * @param {number} timeout
   * @returns {Promise<any>}
   */
  async #call(method, args, timeout) {
    if (!this.#rpc) {
      this.#channel = new BroadcastChannel(this.#channelName);
      this.#rpc = new RpcChannel(this.#channel);
    }

    const request = this.#rpc.callMethod(method, args);

    /** @type {ReturnType<typeof setTimeout> | undefined} */
    let timer;
    let timedOut = false;
    const deadline = new Promise((_, reject) => {
      timer = setTimeout(() => {
        timedOut = true;
        reject(new Error(NOT_REACHABLE));
      }, timeout);
    });

    try {
      return await Promise.race([request, deadline]);
    } catch (err) {
      // Drop the channel after a timeout: the pending request will never
      // settle, and a later call should get a fresh channel (and a fresh
      // chance to reach a newly-opened leader tab).
      if (timedOut) this.close();
      throw err;
    } finally {
      clearTimeout(timer);
    }
  }
}

/**
 * @typedef {Object} StorageActions
 * @property {() => StorageStatus | Promise<StorageStatus>} storageStatus
 * @property {() => StorageFile[] | Promise<StorageFile[]>} storageListFiles
 * @property {(name: string) => Uint8Array | Promise<Uint8Array>} storageRead
 * @property {(name: string, expireIn?: number) => string | null | Promise<string | null>} storageFileURL
 * @property {(name: string, bytes: Uint8Array, mime: string) => { cid: string } | null | Promise<{ cid: string } | null>} storageUploadFile
 * @property {(name: string) => void | Promise<void>} storageDeleteFile
 */
