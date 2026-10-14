import { createClient, parseURI } from "~/components/input/s3/common.js";

/**
 * @import { Bucket } from "@specs/components/input/s3/types.d.ts"
 */

////////////////////////////////////////////
// S3 API
////////////////////////////////////////////

/**
 * Upload a file to an S3-compatible bucket. Overwrites any existing object at
 * the same key — safe because uploads are content-addressed (the filename is
 * `<CID>.<ext>`), so the same bytes always map to the same key (overwrite is a
 * no-op) and different bytes get a different key. This makes re-uploads
 * idempotent.
 *
 * @param {Bucket} bucket
 * @param {string} destinationKey Full object key (e.g. "Music/song.mp3").
 * @param {File} file
 * @returns {Promise<{ key: string }>} The object key of the uploaded file.
 */
export async function uploadFile(bucket, destinationKey, file) {
  const client = createClient(bucket);

  try {
    const bytes = new Uint8Array(await file.arrayBuffer());
    await client.putObject(destinationKey, bytes, {
      size: bytes.byteLength,
      metadata: file.type ? { "Content-Type": file.type } : undefined,
    });
  } catch (err) {
    const detail = err instanceof Error && err.message
      ? ` (${err.message})`
      : "";
    throw new Error(
      `Failed to upload "${file.name}" to S3 bucket "${bucket.bucketName}"${detail}`,
    );
  }

  return { key: destinationKey };
}

/**
 * Delete an object from an S3-compatible bucket.
 *
 * @param {Bucket} bucket
 * @param {string} key Full object key (e.g. "Music/song.mp3").
 * @returns {Promise<boolean>}
 */
export async function deleteFile(bucket, key) {
  const client = createClient(bucket);

  try {
    await client.deleteObject(key);
  } catch (err) {
    const detail = err instanceof Error && err.message
      ? ` (${err.message})`
      : "";
    throw new Error(
      `Failed to delete "${key}" from S3 bucket "${bucket.bucketName}"${detail}`,
    );
  }

  return true;
}

////////////////////////////////////////////
// PATH HELPERS
////////////////////////////////////////////

/**
 * Resolve the destination key for an upload.
 *
 * If `path` is provided it is used as-is (a full object key). Otherwise the
 * file is placed inside the bucket's configured path — the prefix the matching
 * input component lists objects under.
 *
 * @param {Bucket} bucket
 * @param {File} file
 * @param {string} [path]
 * @returns {string}
 */
export function resolveDestinationPath(bucket, file, path) {
  if (path) return path;

  const dir = bucket.path.replace(/^\/+|\/+$/g, "");
  return dir ? `${dir}/${file.name}` : file.name;
}

/**
 * Extract the bucket config from an account URI.
 * Throws if the URI is not a valid S3 URI.
 *
 * @param {string} uri
 * @returns {Bucket}
 */
export function bucketFromURI(uri) {
  const parsed = parseURI(uri);
  if (!parsed) {
    throw new Error(`Invalid S3 URI: ${uri}`);
  }
  return parsed.bucket;
}

/**
 * Extract the bucket config and object key from a track URI.
 * Throws if the URI is not a valid S3 file URI.
 *
 * @param {string} uri
 * @returns {{ bucket: Bucket; key: string }}
 */
export function bucketAndKeyFromURI(uri) {
  const parsed = parseURI(uri);
  if (!parsed) {
    throw new Error(`Invalid S3 URI: ${uri}`);
  }
  if (!parsed.path) {
    throw new Error(`Invalid S3 file URI: ${uri}`);
  }

  return { bucket: parsed.bucket, key: parsed.path };
}
