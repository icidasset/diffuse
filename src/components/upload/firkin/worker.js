import { ostiary, rpc } from "~/common/worker.js";
import {
  buildURI,
  deleteFile,
  parseURI,
  safeFileName,
  uploadFile,
} from "~/common/firkin/files.js";
import { getSessionClient } from "~/common/firkin/session.js";

/**
 * @import { UploadActions as Actions } from "@specs/components/upload/types.d.ts"
 */

const NOT_CONNECTED =
  "Firkin is not connected. Sign in on the Connect → Firkin page first.";

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['consult']}
 */
export async function consult(uriOrScheme) {
  if (!uriOrScheme.includes("://")) {
    return { supported: true, consult: "undetermined" };
  }

  try {
    const client = await getSessionClient();
    return { supported: true, consult: client ? "yes" : "no" };
  } catch {
    return { supported: true, consult: "unsure" };
  }
}

/**
 * @type {Actions['upload']}
 */
export async function upload({ file, uri }) {
  const target = parseURI(uri);
  const name = target?.name ?? safeFileName(file.name);

  const client = await getSessionClient();
  if (!client) throw new Error(NOT_CONNECTED);

  const bytes = new Uint8Array(await file.arrayBuffer());
  await uploadFile(client.rb, name, bytes);

  return buildURI(name);
}

/**
 * @type {Actions['delete']}
 */
export async function deleteFn(uri) {
  const parsed = parseURI(uri);
  if (!parsed) return;

  const client = await getSessionClient();
  if (!client) throw new Error(NOT_CONNECTED);

  await deleteFile(client.rb, parsed.name);
}

/**
 * @type {Actions['createSource']}
 */
// deno-lint-ignore require-await
export async function createSource(_args) {
  // The Firkin source is the signed-in account itself; there is no OAuth flow
  // that could mint one from here.
  throw new Error(
    "Firkin does not use OAuth. Sign in on the Connect → Firkin page first, then select it for syncing.",
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
