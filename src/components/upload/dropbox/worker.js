import * as TID from "@atcute/tid";
import { ostiary, rpc } from "~/common/worker.js";
import {
  buildURI,
  checkAccessCached,
  parseURI,
} from "~/components/input/dropbox/common.js";
import {
  accountFromURI,
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

  const accessible = await checkAccessCached(parsed.accessToken);
  return { supported: true, consult: accessible };
}

/**
 * @type {Actions['upload']}
 */
export async function upload({ file, uri, path }) {
  const account = accountFromURI(uri);
  const destinationPath = resolveDestinationPath(account, file, path);

  const uploaded = await uploadFile(
    account.accessToken,
    destinationPath,
    file,
  );

  if (!uploaded) {
    throw new Error(`Failed to upload "${file.name}" to Dropbox`);
  }

  return buildURI(account, uploaded.path_lower);
}

/**
 * @type {Actions['delete']}
 */
export async function deleteFn(uri) {
  const parsed = parseURI(uri);
  if (!parsed || parsed.path === "/") {
    throw new Error(`Invalid Dropbox file URI: ${uri}`);
  }

  await deleteFile(parsed.accessToken, parsed.path);
}

/**
 * @type {Actions['createSource']}
 */
export async function createSource({ accessToken, directoryPath }) {
  const uri = buildURI({ accessToken, directoryPath });
  const now = new Date().toISOString();
  return {
    $type: "sh.diffuse.output.track",
    id: TID.now(),
    createdAt: now,
    updatedAt: now,
    kind: "placeholder",
    uri,
  };
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
