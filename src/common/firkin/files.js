import { base64url } from "iso-base/rfc4648";
import { CID } from "multiformats/cid";

/**
 * @import { Firkin } from "@tokono.ma/firkin"
 */

/**
 * Files stored on Firkin, listed from the account's namespaced registry, plus
 * generic CID-addressed blobs.
 *
 * Account files are raw Firkin blobs; the registry name is a base64url-encoded
 * file name (Firkin registry names reject spaces and several other filename
 * characters) pointing at its CID.
 *
 * Generic blobs use `firkin://<host>/blob/<cid>` and resolve anonymously via
 * RASL (`https://<host>/.well-known/rasl/<cid>`) by default; the instance must
 * have `RASL=true`. A `?token=` query parameter opts into an authenticated
 * `/v1/blob/<cid>?token=` read instead (for private blobs or deployments with
 * RASL disabled). Tokens are bearer secrets and are not refreshed
 * automatically, so prefer RASL or account mode when possible.
 */

/** Path segment for a generic, CID-addressed blob. */
const BLOB_DIR = "blob";

/** URI scheme for files stored on Firkin. */
export const SCHEME = "firkin";

/** The single URI representing the current Firkin account. */
export const SOURCE_URI = `${SCHEME}://me`;

/** Path segment used in file URIs (user-facing). */
export const URI_DIR = "files";

/** Registry namespace holding `encoded-file-name → blob CID`. */
export const FILES_NAMESPACE = "diffuse.files";

/** Firkin's maximum registry-name length. */
const MAX_REGISTRY_NAME_LENGTH = 512;

/**
 * How long a CID-scoped Firkin read URL stays valid.
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
 */

/**
 * Encode a file name as a Firkin registry name.
 *
 * Registry names are restricted to `A–Z a–z 0–9 . _ - ~ @ + :`, so a
 * base64url encoding keeps arbitrary (Unicode, spaces, punctuation) filenames
 * addressable.
 *
 * @param {string} name
 */
export function encodeFileName(name) {
  const encoded = base64url.encode(new TextEncoder().encode(name));

  if (encoded.length > MAX_REGISTRY_NAME_LENGTH) {
    throw new Error(`File name is too long for Firkin: ${name}`);
  }

  return encoded;
}

/**
 * @param {string} encoded
 */
export function decodeFileName(encoded) {
  return new TextDecoder().decode(base64url.decode(encoded));
}

/**
 * URI of a file inside the Firkin account.
 *
 * @param {string} name
 */
export function buildURI(name) {
  return `${SOURCE_URI}/${URI_DIR}/${encodeURIComponent(name)}`;
}

/**
 * @typedef {Object} BlobURI
 * @property {string} host - Firkin API host (and optional port).
 * @property {string} cid
 * @property {string} name - Display name, defaults to the CID.
 * @property {string} [token] - Optional authenticated read token.
 */

/**
 * Build a generic CID-addressed blob URI.
 *
 * @param {{ host: string; cid: string; name?: string; token?: string }} blob
 */
export function buildBlobURI({ host, cid, name, token }) {
  const params = new URLSearchParams();
  if (name && name !== cid) params.set("name", name);
  if (token) params.set("token", token);

  const query = params.toString();
  return `firkin://${host}/${BLOB_DIR}/${cid}${query ? `?${query}` : ""}`;
}

/**
 * Parse a generic `firkin://<host>/blob/<cid>` URI. Returns null for account
 * file URIs, other schemes, and malformed CIDs.
 *
 * @param {string} uri
 * @returns {BlobURI | null}
 */
export function parseBlobURI(uri) {
  // Parse the authority/path ourselves: browsers and Deno disagree on how
  // `new URL` handles the authority of a non-special scheme like `firkin:`.
  const match =
    /^firkin:\/\/([^/?#]+)\/blob\/([^/?#]+)(?:\?([^#]*))?(?:#.*)?$/.exec(uri);
  if (!match) return null;

  const host = match[1];
  if (!host || host === "me" || /\s/.test(host)) return null;

  const cid = match[2];
  try {
    CID.parse(cid);
  } catch {
    return null;
  }

  const params = new URLSearchParams(match[3] ?? "");

  return {
    host,
    cid,
    name: params.get("name") || cid,
    token: params.get("token") || undefined,
  };
}

/**
 * Media URL for a generic blob.
 *
 * RASL is the default: it is anonymous, stateless, cacheable and
 * content-addressed. A `token` in the URI switches to the authenticated blob
 * route, which is needed for private blobs or deployments with RASL disabled.
 *
 * @param {BlobURI} blob
 */
export function blobURL(blob) {
  const origin = `https://${blob.host}`;

  if (blob.token) {
    return `${origin}/v1/blob/${blob.cid}?token=${
      encodeURIComponent(blob.token)
    }`;
  }

  return `${origin}/.well-known/rasl/${blob.cid}`;
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

/**
 * Strip path/control characters from a browser-provided file name.
 *
 * @param {string} name
 */
export function safeFileName(name) {
  const base = name.split("/").pop() ?? "";
  const cleaned = base.replace(/[\u0000-\u001f]/g, "").trim();
  return cleaned.length > 0 ? cleaned : "audio";
}

/**
 * Every file in the account's file registry.
 *
 * @param {Firkin} rb
 * @returns {Promise<StorageFile[]>}
 */
export async function listFiles(rb) {
  const names = await rb.registryNames(FILES_NAMESPACE);
  if (!names) return [];

  /** @type {StorageFile[]} */
  const files = [];

  for (const [encoded, cid] of Object.entries(names)) {
    try {
      files.push({ name: decodeFileName(encoded), cid });
    } catch {
      // Ignore entries that were not written by encodeFileName.
    }
  }

  return files;
}

/**
 * Resolve one file pointer, or `undefined` when it is not registered.
 *
 * @param {Firkin} rb
 * @param {string} name
 * @returns {Promise<StorageFile | undefined>}
 */
export async function getFile(rb, name) {
  const cid = await rb.registryGet(FILES_NAMESPACE, encodeFileName(name));
  return cid ? { name, cid } : undefined;
}

/**
 * Read a file's bytes.
 *
 * @param {Firkin} rb
 * @param {string} name
 * @returns {Promise<Uint8Array>}
 */
export async function readFile(rb, name) {
  const file = await getFile(rb, name);
  if (!file) throw new Error(`File not found on Firkin: ${name}`);

  return await rb.get(file.cid);
}

/**
 * A CID-scoped, range-capable Firkin URL for a file — what media elements and
 * the metadata workers use.
 *
 * @param {Firkin} rb
 * @param {string} name
 * @param {number} [expireIn]
 * @returns {Promise<string>}
 */
export async function fileURL(rb, name, expireIn) {
  const file = await getFile(rb, name);
  if (!file) throw new Error(`File not found on Firkin: ${name}`);

  return await rb.readUrl(file.cid, {
    expireIn: expireIn ?? FILE_URL_TTL_SECONDS,
  });
}

/**
 * Upload the bytes as a raw Firkin blob and point the file name at it.
 *
 * @param {Firkin} rb
 * @param {string} name
 * @param {Uint8Array} bytes
 * @returns {Promise<{ cid: string }>}
 */
export async function uploadFile(rb, name, bytes) {
  const registryName = encodeFileName(name);
  const previous = await rb.registryGet(FILES_NAMESPACE, registryName);

  // Audio bytes stay in Firkin; the registry only holds the pointer.
  const { cid } = await rb.upload(bytes, { codec: 0x55 });
  await rb.registrySet(FILES_NAMESPACE, registryName, cid);

  if (previous && previous !== cid) {
    await deleteIfUnreferenced(rb, previous);
  }

  return { cid };
}

/**
 * Remove a file's registry pointer and detach the blob's account claim when
 * no other file name references it.
 *
 * @param {Firkin} rb
 * @param {string} name
 */
export async function deleteFile(rb, name) {
  const registryName = encodeFileName(name);
  const cid = await rb.registryGet(FILES_NAMESPACE, registryName);
  if (!cid) return;

  await rb.registryDelete(FILES_NAMESPACE, registryName);
  await deleteIfUnreferenced(rb, cid);
}

/**
 * @param {Firkin} rb
 * @param {string} cid
 */
async function deleteIfUnreferenced(rb, cid) {
  const names = await rb.registryNames(FILES_NAMESPACE);
  const referenced = names !== null &&
    Object.values(names).some((value) => value === cid);

  if (!referenced) {
    await rb.delete(cid).catch(() => {});
  }
}
