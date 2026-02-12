import { CID } from "multiformats/cid";
import { sha256 } from "multiformats/hashes/sha2";

/**
 * @param {number} code
 * @param {Uint8Array} data
 */
export async function create(code, data) {
  const hash = await sha256.digest(data);
  const cid = CID.create(1, code, hash);
  console.log(cid.toString());

  return cid.toString();
}
