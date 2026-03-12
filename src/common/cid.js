import * as CID from "@atcute/cid";
import { equals, toSha256 } from "@atcute/uint8array";

/**
 * @param {0x55 | 0x71} code
 * @param {Uint8Array<any>} data
 */
export async function create(code, data) {
  const cid = await CID.create(code, data);
  return CID.toString(cid);
}

/**
 * @param {Uint8Array<any>} data
 * @param {string} expected
 */
export async function verify(data, expected) {
  const expectedCid = CID.fromString(expected);
  const digest = await toSha256(data);

  return equals(digest, expectedCid.digest.contents);
}
