import * as URI from "fast-uri";
import { ostiary, rpc, workerProxy } from "~/common/worker.js";

/**
 * @import {UploadActions} from "@specs/components/upload/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {Actions} from "@specs/components/configurator/upload/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {ActionsWithTunnel<Actions>['consult']}
 */
export async function consult({ data, ports }) {
  const fileUriOrScheme = data;
  const scheme = fileUriOrScheme.includes(":")
    ? URI.parse(fileUriOrScheme).scheme || fileUriOrScheme
    : fileUriOrScheme;

  const upload = grabUploader(scheme, ports);
  if (!upload) {
    return { supported: false, reason: "Unsupported scheme" };
  }

  return await upload.consult(fileUriOrScheme);
}

/**
 * @type {ActionsWithTunnel<Actions>['createSource']}
 */
export async function createSource({ data, ports }) {
  const upload = grabUploader(data.scheme, ports);
  if (!upload) {
    throw new Error(`Unsupported scheme: ${data.scheme}`);
  }

  return await upload.createSource(data);
}

/**
 * @type {ActionsWithTunnel<Actions>['delete']}
 */
export async function deleteFn({ data, ports }) {
  const uri = data;
  const scheme = uri.split(":", 1)[0];
  const upload = grabUploader(scheme, ports);
  if (!upload) {
    throw new Error(`Unsupported scheme: ${scheme}`);
  }

  await upload.delete(uri);
}

/**
 * @type {ActionsWithTunnel<Actions>['upload']}
 */
export async function upload({ data, ports }) {
  const scheme = data.uri.split(":", 1)[0];
  const upload = grabUploader(scheme, ports);
  if (!upload) {
    throw new Error(`Unsupported scheme: ${scheme}`);
  }

  return await upload.upload(data);
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

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {string} scheme
 * @param {Record<string, MessagePort>} ports
 * @returns {ProxiedActions<UploadActions> | null}
 */
function grabUploader(scheme, ports) {
  const port = ports[scheme];
  if (!port) return null;

  return workerProxy(() => {
    port.start();
    return port;
  });
}
