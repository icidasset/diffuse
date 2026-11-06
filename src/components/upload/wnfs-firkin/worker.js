import { ostiary, rpc } from "~/common/worker.js";
import {
  buildURI,
  mimeType,
  parseURI,
  StorageClient,
} from "~/components/output/bytes/wnfs/files.js";

/**
 * @import { UploadActions as Actions } from "@specs/components/upload/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const params = new URL(globalThis.location.href).searchParams;

const client = new StorageClient({
  group: params.get("group") || undefined,
  namespace: params.get("namespace") || undefined,
});

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
    const status = await client.status();
    return { supported: true, consult: status.connected ? "yes" : "no" };
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
  const mime = file.type || mimeType(name);

  const bytes = new Uint8Array(await file.arrayBuffer());
  await client.uploadFile(name, bytes, mime);

  return buildURI(name);
}

/**
 * @type {Actions['delete']}
 */
export async function deleteFn(uri) {
  const parsed = parseURI(uri);
  if (!parsed) return;

  await client.deleteFile(parsed.name);
}

/**
 * @type {Actions['createSource']}
 */
// deno-lint-ignore require-await
export async function createSource(_args) {
  // The Firkin + WNFS source is the signed-in output itself; there is no
  // OAuth flow that could mint one from here.
  throw new Error(
    "Firkin + WNFS does not use OAuth. Sign in on the Connect → Firkin + WNFS page first, then select it for syncing.",
  );
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {string} name
 */
function safeFileName(name) {
  const base = name.split("/").pop() ?? "";
  const cleaned = base.replace(/[\u0000-\u001f]/g, "").trim();
  return cleaned.length > 0 ? cleaned : "audio";
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
