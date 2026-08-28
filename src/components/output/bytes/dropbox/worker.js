import { ostiary, rpc } from "~/common/worker.js";

import { getFile, putFile } from "./common.js";

/**
 * @import {DropboxOutputWorkerActions} from "@specs/components/output/bytes/dropbox/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {DropboxOutputWorkerActions["get"]}
 */
export async function get({ refreshToken, name }) {
  try {
    return await getFile(refreshToken, name);
  } catch (err) {
    console.error("Failed to get Dropbox file:", err);
    return undefined;
  }
}

/**
 * @type {DropboxOutputWorkerActions["put"]}
 */
export async function put({ refreshToken, data, name }) {
  try {
    await putFile(refreshToken, name, data);
  } catch (err) {
    console.error("Failed to put Dropbox file:", err);
    throw err;
  }
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    get,
    put,
  });
});
