import * as CID from "@atcute/cid";
import { equals, toSha256 } from "@atcute/uint8array";

/**
 * @param {0x55 | 0x71} code
 * @param {Uint8Array<any>} data
 *
 * @example Returns a non-empty base32 CID string, consistent for same input, different for different inputs
 * ```js
 * import { create } from "~/common/cid.js";
 *
 * const data = new TextEncoder().encode("hello world");
 * const cid = await create(0x55, data);
 * if (typeof cid !== "string" || cid.length === 0) throw new Error("CID should be a non-empty string");
 * if (!/^[a-z2-7]+$/.test(cid)) throw new Error("CID should be base32-encoded");
 *
 * const cid2 = await create(0x55, data);
 * if (cid !== cid2) throw new Error("same input should produce same CID");
 *
 * const cidDiff = await create(0x55, new TextEncoder().encode("world"));
 * if (cid === cidDiff) throw new Error("different input should produce different CID");
 *
 * const cidCodec = await create(0x71, data);
 * if (cid === cidCodec) throw new Error("different codec should produce different CID");
 * ```
 */
export async function create(code, data) {
  const cid = await CID.create(code, data);
  return CID.toString(cid);
}

/**
 * @param {Uint8Array<any>} data
 * @param {string} expected
 *
 * @example Returns true for matching data and false for mismatched data
 * ```js
 * import { create, verify } from "~/common/cid.js";
 *
 * const data = new TextEncoder().encode("hello");
 * const cid = await create(0x55, data);
 * if (!await verify(data, cid)) throw new Error("should verify matching data");
 * if (await verify(new TextEncoder().encode("world"), cid)) throw new Error("should not verify mismatched data");
 * ```
 */
export async function verify(data, expected) {
  const expectedCid = CID.fromString(expected);
  const digest = await toSha256(data);

  return equals(digest, expectedCid.digest.contents);
}
