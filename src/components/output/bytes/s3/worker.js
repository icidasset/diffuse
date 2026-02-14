import { createClient } from "@components/input/s3/common.js";
import { ostiary, rpc } from "@common/worker.js";

import { OBJECT_PREFIX } from "./constants.js";

/**
 * @import {S3OutputWorkerActions} from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {S3OutputWorkerActions["get"]}
 */
export async function get({ bucket, name }) {
  const client = createClient(bucket);
  const key = `${OBJECT_PREFIX}${name}`;

  try {
    const response = await client.getObject(key);
    return await response.bytes();
  } catch (err) {
    // Object doesn't exist yet, return undefined
    return undefined;
  }
}

/**
 * @type {S3OutputWorkerActions["put"]}
 */
export async function put({ bucket, data, name }) {
  const client = createClient(bucket);
  const key = `${OBJECT_PREFIX}${name}`;

  await client.putObject(key, data);
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
