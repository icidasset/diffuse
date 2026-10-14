import { ostiary, rpc } from "~/common/worker.js";
import {
  buildURI,
  consultBucketCached,
  parseURI,
} from "~/components/input/s3/common.js";
import {
  bucketAndKeyFromURI,
  bucketFromURI,
  deleteFile,
  resolveDestinationPath,
  uploadFile,
} from "./common.js";

/**
 * @import { UploadActions as Actions } from "@specs/components/upload/types.d.ts";
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const accessible = await consultBucketCached(parsed.bucket);
  return { supported: true, consult: accessible };
}

/**
 * @type {Actions['upload']}
 */
export async function upload({ file, uri, path }) {
  const bucket = bucketFromURI(uri);
  const destinationKey = resolveDestinationPath(bucket, file, path);

  await uploadFile(bucket, destinationKey, file);

  return buildURI(bucket, destinationKey);
}

/**
 * @type {Actions['delete']}
 */
export async function deleteFn(uri) {
  const { bucket, key } = bucketAndKeyFromURI(uri);
  await deleteFile(bucket, key);
}

/**
 * @type {Actions['createSource']}
 */
export async function createSource(_args) {
  // S3 buckets are connected (with their credentials) via the Connect page —
  // there's no OAuth flow that could mint a new source here.
  throw new Error(
    "S3 does not use OAuth. Connect a bucket on the Connect page first, then select it for syncing.",
  );
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    consult,
    upload,
    delete: deleteFn,
    createSource,
  });
});
