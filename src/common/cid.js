import { equals } from "iso-base/utils";
import { CID } from "multiformats/cid";
import { sha256 } from "multiformats/hashes/sha2";

/**
 * @param {number} code
 * @param {Uint8Array} data
 */
export async function create(code, data) {
  const hash = await sha256.digest(data);
  const cid = CID.create(1, code, hash);

  return cid.toString();
}

/**
 * @param {Uint8Array} data
 * @param {string} expected
 */
export async function verify(data, expected) {
  const expectedCid = CID.parse(expected);
  const hash = await sha256.digest(data);
  return equals(expectedCid.multihash.bytes, hash.bytes);
}
