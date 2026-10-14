import { ostiary, rpc } from "~/common/worker.js";
import {
  buildURI,
  checkAccessCached,
  parseURI,
} from "~/components/input/webdav/common.js";
import {
  deleteFile,
  resolveDestinationPath,
  serverAndPathFromURI,
  serverFromURI,
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

  const accessible = await checkAccessCached(parsed.server);
  return { supported: true, consult: accessible };
}

/**
 * @type {Actions['upload']}
 */
export async function upload({ file, uri, path }) {
  const server = serverFromURI(uri);
  const destinationPath = resolveDestinationPath(server, file, path);

  await uploadFile(server, destinationPath, file);

  // Percent-encode the path (per segment) so the returned URI matches what
  // the matching input component's listing produces (PROPFIND hrefs are
  // percent-encoded).
  const encodedPath = destinationPath.split("/").map(encodeURIComponent).join(
    "/",
  );
  return buildURI(server, encodedPath);
}

/**
 * @type {Actions['delete']}
 */
export async function deleteFn(uri) {
  const { server, path } = serverAndPathFromURI(uri);
  await deleteFile(server, path);
}

/**
 * @type {Actions['createSource']}
 */
// deno-lint-ignore require-await
export async function createSource(_args) {
  // WebDAV servers are connected (with their credentials) via the Connect
  // page — there's no OAuth flow that could mint a new source here.
  throw new Error(
    "WebDAV does not use OAuth. Connect a server on the Connect page first, then select it for syncing.",
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
