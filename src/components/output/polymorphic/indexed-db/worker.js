import * as IDB from "idb-keyval";

import { IDB_PREFIX } from "./constants.js";
import { ostiary, rpc } from "@common/worker.js";

/**
 * @import {OutputWorkerActions} from "@components/output/types.d.ts";
 * @import {SupportedDataTypes} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {OutputWorkerActions<SupportedDataTypes>["get"]}
 */
export async function get({ name }) {
  return await IDB.get(`${IDB_PREFIX}/${name}`);
}

/**
 * @type {OutputWorkerActions<SupportedDataTypes>["put"]}
 */
export async function put({ data, name }) {
  return await IDB.set(`${IDB_PREFIX}/${name}`, data);
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
