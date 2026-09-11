import * as CID from "@atcute/cid";
import { fromUint8Array } from "@atcute/car";
import { sha256 } from "@noble/hashes/sha2.js";

const textDecoder = new TextDecoder();

/**
 * Computes a CID string synchronously (SHA-256 via noble), matching
 * `@atcute/cid` `create()` which hashes with SHA-256.
 *
 * @param {0x55 | 0x71} code
 * @param {Uint8Array} data
 * @returns {string}
 *
 * @example Produces a stable base32 CID that decodes back to a valid CID string
 * ```js
 * import { createSyncCID } from "~/common/tiles.js";
 * import * as CID from "@atcute/cid";
 *
 * const cid = createSyncCID(0x55, new TextEncoder().encode("hello"));
 * const parsed = CID.fromString(cid);
 * if (parsed.codec !== 0x55) throw new Error("should be raw codec");
 * ```
 */
export function createSyncCID(code, data) {
  const digest = sha256(new Uint8Array(data));
  return CID.toString(CID.fromDigest(code, digest));
}

/**
 * @typedef {Record<string, { src: unknown; "content-type"?: string }>} TileResources
 * @typedef {{ html?: string; uri?: string; resources?: TileResources; blocks?: Record<string, unknown> }} TileSource
 */

/**
 * @param {string} text
 * @returns {Uint8Array}
 *
 * @example Decodes base64 back to the original bytes
 * ```js
 * import { decodeBase64, encodeBase64 } from "~/common/tiles.js";
 *
 * const original = new TextEncoder().encode("hello 🙂");
 * const roundtrip = decodeBase64(encodeBase64(original));
 * if (roundtrip.length !== original.length) throw new Error("length should match");
 * if (roundtrip.some((b, i) => b !== original[i])) throw new Error("bytes should match");
 * ```
 */
export function decodeBase64(text) {
  const binary = atob(text);
  const bytes = new Uint8Array(binary.length);
  for (let i = 0; i < binary.length; i++) bytes[i] = binary.charCodeAt(i);
  return bytes;
}

/**
 * @param {Uint8Array<ArrayBufferLike>} bytes
 * @returns {string}
 *
 * @example Returns a base64 string that decodes back to the input
 * ```js
 * import { encodeBase64, decodeBase64 } from "~/common/tiles.js";
 *
 * const bytes = new TextEncoder().encode("data:🌱");
 * const encoded = encodeBase64(bytes);
 * if (typeof encoded !== "string" || encoded.length === 0) throw new Error("should be a non-empty string");
 * const decoded = decodeBase64(encoded);
 * if (decoded.length !== bytes.length) throw new Error("roundtrip length should match");
 * ```
 */
export function encodeBase64(bytes) {
  let binary = "";
  // Chunk to avoid stack overflow when spreading very large arrays into
  // String.fromCharCode. 0x8000 keeps the loop fast without overflowing.
  const chunkSize = 0x8000;
  for (let i = 0; i < bytes.length; i += chunkSize) {
    binary += String.fromCharCode(...bytes.subarray(i, i + chunkSize));
  }
  return btoa(binary);
}

// Synchronous raw DEFLATE (RFC 1951). Needed so block encoding/decoding and the
// legacy-facet migration can run in synchronous (signal-using) paths without the
// async CompressionStream. Both paths here are raw deflate (no zlib wrapper).

const LENGTH_EXTRA = [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0];
const LENGTH_BASE = [3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258];
const DIST_BASE = [1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577];
const DIST_EXTRA = [0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13];

/** Fixed literal/length code lengths (RFC 1951 3.2.6). */
const FIXED_LIT_LENS = (() => {
  const lens = new Uint8Array(288);
  lens.fill(8, 0, 144);
  lens.fill(9, 144, 256);
  lens.fill(7, 256, 280);
  lens.fill(8, 280, 288);
  return lens;
})();

/**
 * Canonical Huffman encoder codes from bit lengths, indexed by symbol.
 *
 * @param {Uint8Array} lens
 * @returns {{ code: Int32Array, bits: Uint8Array } | null}
 */
function canonicalCodes(lens) {
  const max = lens.reduce((m, l) => (l > m ? l : m), 0);
  const count = new Uint32Array(max + 1);
  for (const l of lens) if (l) count[l]++;
  let code = 0;
  const next = new Uint32Array(max + 1);
  for (let bits = 1; bits <= max; bits++) {
    code = (code + count[bits - 1]) << 1;
    next[bits] = code;
  }
  const out = new Int32Array(lens.length);
  const bits = new Uint8Array(lens.length);
  for (let n = 0; n < lens.length; n++) {
    const l = lens[n];
    if (l) {
      out[n] = next[l]++;
      bits[n] = l;
    }
  }
  return { code: out, bits };
}

const FIXED_LIT_CODES = /** @type {{ code: Int32Array, bits: Uint8Array }} */ (canonicalCodes(FIXED_LIT_LENS));
const FIXED_DIST_CODES = /** @type {{ code: Int32Array, bits: Uint8Array }} */ (
  canonicalCodes(new Uint8Array(32).fill(5))
);

/**
 * Build canonical-decode tables ({@code count}/{@code symbol}) from bit lengths,
 * following zlib puff's construct(). Returns null for an invalid code set.
 *
 * @param {Uint8Array} lens
 * @param {number} max
 * @returns {{ count: Uint16Array, symbol: Uint16Array } | null}
 */
function constructHuffman(lens, max) {
  const count = new Uint16Array(max + 1);
  for (const l of lens) if (l) count[l]++;
  let left = 1;
  for (let len = 1; len <= max; len++) {
    left = (left << 1) - count[len];
    if (left < 0) return null;
  }
  if (left > 1) return null;

  const symbol = new Uint16Array(lens.length);
  const offs = new Uint16Array(max + 1);
  let off = 0;
  for (let len = 1; len < max; len++) offs[len + 1] = off += count[len];
  for (let sym = 0; sym < lens.length; sym++) {
    const len = lens[sym];
    if (len) symbol[offs[len]++] = sym;
  }
  return { count, symbol };
}

const FIXED_LIT_DECODE = /** @type {{ count: Uint16Array, symbol: Uint16Array }} */ (constructHuffman(FIXED_LIT_LENS, 15));
const FIXED_DIST_DECODE = /** @type {{ count: Uint16Array, symbol: Uint16Array }} */ (
  constructHuffman(new Uint8Array(32).fill(5), 15)
);

/**
 * Synchronously compresses bytes to raw DEFLATE (RFC 1951): LZ77 matching plus
 * the fixed-Huffman code set. Deterministic and dependency-free.
 *
 * @param {Uint8Array} bytes
 * @returns {Uint8Array}
 *
 * @example Round-trips through inflateSync
 * ```js
 * import { deflateSync, inflateSync } from "~/common/tiles.js";
 *
 * const data = new TextEncoder().encode("hello hello hello hello hello");
 * const roundtrip = inflateSync(deflateSync(data));
 * if (roundtrip.some((b, i) => b !== data[i])) throw new Error("roundtrip should match");
 * if (roundtrip.length !== data.length) throw new Error("length should match");
 * ```
 */
export function deflateSync(bytes) {
  /** @type {number[]} */
  const out = [];
  let buf = 0;
  let nbits = 0;
  /** @type {(v: number, n: number) => void} */
  const put = (v, n) => {
    buf |= v << nbits;
    nbits += n;
    while (nbits >= 8) {
      out.push(buf & 0xff);
      buf >>>= 8;
      nbits -= 8;
    }
  };
  // Write a Huffman code MSB-first (RFC 1951 transmits codes MSB-first,
  // while extra bits and block headers are LSB-first via `put`).
  /** @type {(code: number, len: number) => void} */
  const putCode = (code, len) => {
    for (let k = len - 1; k >= 0; k--) put((code >>> k) & 1, 1);
  };

  const n = bytes.length;
  /** @type {number[]} */ const chain = new Array(65536).fill(-1);
  /** @type {number[]} */ const back = new Array(Math.max(0, n)).fill(-1);
  for (let i = 0; i + 2 < n; i++) {
    const h = (bytes[i] * 7 + bytes[i + 1] * 3 + bytes[i + 2]) & 0xffff;
    back[i] = chain[h];
    chain[h] = i;
  }

  put(1, 1); // BFINAL = 1
  put(1, 2); // BTYPE = 01 (fixed Huffman)

  let i = 0;
  while (i < n) {
    let bestLen = 0;
    let bestDist = 0;
    const maxLen = Math.min(258, n - i);
    if (i >= 3 && maxLen >= 3) {
      const h = (bytes[i] * 7 + bytes[i + 1] * 3 + bytes[i + 2]) & 0xffff;
      let cand = chain[h];
      let steps = 0;
      while (cand >= 0 && cand < i && cand >= i - 32768 && steps < 64) {
        let l = 0;
        while (l < maxLen && bytes[cand + l] === bytes[i + l]) l++;
        if (l > bestLen) {
          bestLen = l;
          bestDist = i - cand;
          if (l === maxLen) break;
        }
        cand = back[cand];
        steps++;
      }
    }
    if (bestLen >= 3) {
      let lc = 0;
      while (LENGTH_BASE[lc + 1] <= bestLen) lc++;
      const symbol = 257 + lc;
      put(FIXED_LIT_CODES.code[symbol], FIXED_LIT_CODES.bits[symbol]);
      putCode(FIXED_LIT_CODES.code[257 + lc], FIXED_LIT_CODES.bits[257 + lc]);
      put(bestLen - LENGTH_BASE[lc], LENGTH_EXTRA[lc]);
      let dc = 0;
      while (DIST_BASE[dc + 1] <= bestDist) dc++;
      putCode(FIXED_DIST_CODES.code[dc], FIXED_DIST_CODES.bits[dc]);
      put(bestDist - DIST_BASE[dc], DIST_EXTRA[dc]);
      i += bestLen;
    } else {
      putCode(FIXED_LIT_CODES.code[bytes[i]], FIXED_LIT_CODES.bits[bytes[i]]);
      i++;
    }
  }
  putCode(FIXED_LIT_CODES.code[256], FIXED_LIT_CODES.bits[256]);
  if (nbits > 0) out.push(buf & 0xff);
  return new Uint8Array(out);
}

/**
 * Synchronously decompresses a raw DEFLATE (RFC 1951) stream, supporting stored,
 * fixed-Huffman, and dynamic-Huffman blocks.
 *
 * @param {Uint8Array} bytes
 * @returns {Uint8Array}
 */
export function inflateSync(bytes) {
  let pos = 0;
  let bitVal = 0;
  let bitLen = 0;
  /** @type {(n: number) => number} */
  const bits = (n) => {
    while (bitLen < n) {
      bitVal |= bytes[pos++] << bitLen;
      bitLen += 8;
    }
    const r = bitVal & ((1 << n) - 1);
    bitVal >>>= n;
    bitLen -= n;
    return r;
  };

  /** @type {number[]} */
  const out = [];
  /** @type {(count: Uint16Array, symbolTable: Uint16Array) => number} */
  const decode = (count, symbolTable) => {
    let code = 0;
    let first = 0;
    let index = 0;
    for (let len = 1; len <= 15; len++) {
      code = (code << 1) | bits(1);
      const cnt = count[len];
      if (code - first < cnt) return symbolTable[index + (code - first)];
      index += cnt;
      first += cnt;
      first <<= 1;
    }
    throw new Error("invalid compressed bitstream");
  };

  /** @type {(lc: Uint16Array, ls: Uint16Array, dc: Uint16Array, ds: Uint16Array) => void} */
  const decodeBlock = (lc, ls, dc, ds) => {
    for (;;) {
      const sym = decode(lc, ls);
      if (sym < 256) {
        out.push(sym);
      } else if (sym === 256) {
        return;
      } else {
        const li = sym - 257;
        const len = LENGTH_BASE[li] + bits(LENGTH_EXTRA[li]);
        const di = decode(dc, ds);
        const dist = DIST_BASE[di] + bits(DIST_EXTRA[di]);
        for (let k = 0; k < len; k++) out.push(out[out.length - dist]);
      }
    }
  };

  let bfinal = 0;
  do {
    bfinal = bits(1);
    const btype = bits(2);
    if (btype === 0) {
      bitVal = 0;
      bitLen = 0;
      const len = bytes[pos] | (bytes[pos + 1] << 8);
      pos += 4;
      for (let k = 0; k < len; k++) out.push(bytes[pos + k]);
      pos += len;
    } else if (btype === 1) {
      decodeBlock(FIXED_LIT_DECODE.count, FIXED_LIT_DECODE.symbol, FIXED_DIST_DECODE.count, FIXED_DIST_DECODE.symbol);
    } else if (btype === 2) {
      const hlit = bits(5) + 257;
      const hdist = bits(5) + 1;
      const hclen = bits(4) + 4;
      const order = [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15];
      const clens = new Uint8Array(19);
      for (let i = 0; i < hclen; i++) clens[order[i]] = bits(3);
      const c = constructHuffman(clens, 7);
      if (!c) throw new Error("invalid code lengths");
      const lens = new Uint8Array(hlit + hdist);
      let idx = 0;
      while (idx < hlit + hdist) {
        const s = decode(c.count, c.symbol);
        if (s < 16) lens[idx++] = s;
        else if (s === 16) {
          const rep = 3 + bits(2);
          const v = lens[idx - 1];
          for (let k = 0; k < rep; k++) lens[idx++] = v;
        } else if (s === 17) {
          idx += 3 + bits(3);
        } else {
          idx += 11 + bits(7);
        }
      }
      const lit = constructHuffman(lens.subarray(0, hlit), 15);
      const dst = constructHuffman(lens.subarray(hlit), 15);
      if (!lit || !dst) throw new Error("invalid dynamic huffman");
      decodeBlock(lit.count, lit.symbol, dst.count, dst.symbol);
    } else {
      throw new Error("invalid deflate block type");
    }
  } while (!bfinal);
  return new Uint8Array(out);
}

/**
 * Serializes a map of CID string to content bytes into the facet's `blocks`
 * object: a plain map of CID string to the most compact representation of that
 * resource's bytes (deflate + base64). Synchronous.
 *
 * @param {Map<string, Uint8Array>} blocks
 * @returns {Record<string, string>}
 *
 * @example Round-trips a cid → bytes map
 * ```js
 * import { encodeBlocks, decodeBlocks } from "~/common/tiles.js";
 * import * as CID from "@atcute/cid";
 *
 * const bytes = new TextEncoder().encode("hola");
 * const cid = await CID.create(0x55, bytes);
 *
 * const encoded = encodeBlocks(new Map([[cid.toString(), bytes]]));
 * if (typeof encoded !== "object" || Object.keys(encoded).length === 0) throw new Error("should be a non-empty map");
 *
 * const decoded = decodeBlocks(encoded);
 * const out = decoded.get(cid.toString());
 * if (!out) throw new Error("should recover the block by its cid");
 * if (new TextDecoder().decode(out) !== "hola") throw new Error("should recover the content");
 * ```
 */
export function encodeBlocks(blocks) {
  /** @type {Record<string, string>} */
  const out = {};
  for (const [cid, bytes] of blocks) out[cid] = encodeBase64(deflateSync(bytes));
  return out;
}

/**
 * Deserializes a facet's `blocks` object (produced by {@link encodeBlocks})
 * back into a map of CID string to content bytes. Synchronous.
 *
 * @param {Record<string, unknown> | undefined} blocks
 * @returns {Map<string, Uint8Array>}
 *
 * @example Returns an empty map for an undefined / empty object
 * ```js
 * import { decodeBlocks } from "~/common/tiles.js";
 *
 * const blocks = decodeBlocks(undefined);
 * if (blocks.size !== 0) throw new Error("empty blocks should decode to an empty map");
 * ```
 */
export function decodeBlocks(blocks) {
  const out = new Map();
  if (!blocks) return out;

  for (const [cid, value] of Object.entries(blocks)) {
    out.set(cid, inflateSync(decodeBase64(String(value ?? ""))));
  }

  return out;
}

/**
 * Converts any of the ways a CID can be represented (a plain base32 string, a
 * decoded CID object, or an atproto `CidLinkWrapper`) into a base32 string.
 *
 * @param {unknown} cid
 * @returns {string | undefined}
 */
function cidToString(cid) {
  if (typeof cid === "string") return cid;
  if (cid == null) return undefined;
  if (typeof /** @type {any} */ (cid).$link === "string") {
    return /** @type {any} */ (cid).$link;
  }
  if (/** @type {any} */ (cid).bytes) return CID.toString(/** @type {any} */ (cid));
  return undefined;
}

/**
 * Parses a CARv1 archive (a `.tile` file) into its MASL header and a map of
 * every block's bytes, keyed by CID string. As specified by the DASL "Tiles in
 * CAR" convention, the CAR header is the MASL metadata (along with the `roots`
 * and `version` fields CAR requires).
 *
 * @param {Uint8Array<ArrayBufferLike>} bytes
 * @returns {{ manifest: Record<string, any>; blocks: Map<string, Uint8Array> }}
 */
export function parseCar(bytes) {
  const car = fromUint8Array(bytes);
  const blocks = new Map();

  for (const block of car) {
    const key = cidToString(block.cid);
    if (key) blocks.set(key, block.bytes);
  }

  return { manifest: car.header.data, blocks };
}

/**
 * Resolves a tile's root (`/`) resource to its decoded HTML, given a MASL
 * `resources` map and a map of blocks. Integrity of the root is content
 * addressed: the resource's `src` CID identifies the exact bytes, so the root
 * HTML can be verified against it later.
 *
 * @param {TileResources | undefined} resources
 * @param {Map<string, Uint8Array>} blocks
 * @returns {{ cid: string; html: string; contentType: string | undefined } | undefined}
 *
 * @example Resolves the root resource by its src cid
 * ```js
 * import { resolveRoot } from "~/common/tiles.js";
 * import * as CID from "@atcute/cid";
 *
 * const data = new TextEncoder().encode("<p>hi</p>");
 * const cid = await CID.create(0x55, data);
 *
 * const root = resolveRoot(
 *   { "/": { src: { $link: cid.toString() }, "content-type": "text/html" } },
 *   new Map([[cid.toString(), data]]),
 * );
 *
 * if (root === undefined) throw new Error("should resolve the root");
 * if (root.html !== "<p>hi</p>") throw new Error("root should be the index html");
 * if (root.cid !== cid.toString()) throw new Error("should expose the root cid");
 * if (root.contentType !== "text/html") throw new Error("should expose the content-type");
 * ```
 */
export function resolveRoot(resources, blocks) {
  const resource = resources?.["/"];
  if (!resource) return undefined;

  const cid = cidToString(resource.src);
  if (!cid) return undefined;

  const bytes = blocks.get(cid);
  if (!bytes) return undefined;

  return {
    cid,
    html: textDecoder.decode(bytes),
    contentType: resource["content-type"],
  };
}

/**
 * Collects a tile's absolute-path resources (paths starting with `/`) from its
 * `resources` map and blocks, returning a map of path to decoded content.
 * Relative paths are intentionally excluded: they keep their built-in meaning
 * against the Diffuse build root (via the page `<base>` element) so facets may
 * still reference shared Diffuse assets.
 *
 * @param {TileResources | undefined} resources
 * @param {Map<string, Uint8Array>} blocks
 * @returns {Map<string, { cid: string; bytes: Uint8Array; contentType: string | undefined }>}
 *
 * @example Collects absolute resources and skips relative ones
 * ```js
 * import { tileResourceEntries } from "~/common/tiles.js";
 * import * as CID from "@atcute/cid";
 *
 * const css = new TextEncoder().encode("body { color: red }");
 * const cssCid = await CID.create(0x55, css);
 * const html = new TextEncoder().encode("<p>hi</p>");
 * const htmlCid = await CID.create(0x55, html);
 *
 * const entries = tileResourceEntries(
 *   {
 *     "/": { src: { $link: htmlCid.toString() }, "content-type": "text/html" },
 *     "/style.css": { src: { $link: cssCid.toString() }, "content-type": "text/css" },
 *     "./local.css": { src: { $link: cssCid.toString() } },
 *   },
 *   new Map([[cssCid.toString(), css], [htmlCid.toString(), html]]),
 * );
 *
 * if (!entries.has("/style.css")) throw new Error("absolute resource should be collected");
 * if (entries.has("./local.css")) throw new Error("relative resource should be skipped");
 * if (entries.get("/style.css")?.contentType !== "text/css") throw new Error("should carry content-type");
 * ```
 */
export function tileResourceEntries(resources, blocks) {
  /** @type {Map<string, { cid: string; bytes: Uint8Array; contentType: string | undefined }>} */
  const out = new Map();
  if (!resources) return out;

  for (const [path, resource] of Object.entries(resources)) {
    if (!path.startsWith("/")) continue;

    const cid = cidToString(resource.src);
    if (!cid) continue;

    const bytes = blocks.get(cid);
    if (!bytes) continue;

    out.set(path, {
      cid,
      bytes,
      contentType: resource["content-type"],
    });
  }

  return out;
}

/**
 * Resolves the root HTML of an inline tile facet: the facet's own `resources`
 * map plus a `blocks` map of CID to content bytes (see {@link encodeBlocks}).
 * The facet record itself is the MASL manifest.
 *
 * @param {TileResources | undefined} resources
 * @param {Record<string, unknown> | undefined} blocks
 * @returns {Promise<{ cid: string; html: string; contentType: string | undefined } | undefined>}
 *
 * @example Resolves HTML from an inline MASL manifest plus blocks
 * ```js
 * import { resolveInlineTile, encodeBlocks } from "~/common/tiles.js";
 * import * as CID from "@atcute/cid";
 *
 * const data = new TextEncoder().encode("<p>hello</p>");
 * const cid = await CID.create(0x55, data);
 *
 * const blocks = await encodeBlocks(new Map([[cid.toString(), data]]));
 * const root = await resolveInlineTile(
 *   { "/": { src: { $link: cid.toString() }, "content-type": "text/html" } },
 *   blocks,
 * );
 *
 * if (root === undefined) throw new Error("should resolve the root");
 * if (root.html !== "<p>hello</p>") throw new Error("root should decode to the html");
 * if (root.cid !== cid.toString()) throw new Error("should report the root cid");
 * if (root.contentType !== "text/html") throw new Error("should surface the content-type");
 * ```
 */
export async function resolveInlineTile(resources, blocks) {
  const decoded = await decodeBlocks(blocks);
  return resolveRoot(resources, decoded);
}

/**
 * Rewrites absolute URL specifiers in ES module source (`import "/x.js"`,
 * `import … from "/x.js"`, `export … from "/x.js"`, `import("/x.js")`) using
 * the provided resolver. Relative specifiers are left untouched, so they keep
 * their meaning against the importing module. This lets a tile's modules import
 * each other by path when served as Blob URLs (where import maps do not apply).
 *
 * @param {string} source
 * @param {(specifier: string) => string | undefined} resolve - Maps an absolute specifier to its replacement URL, or `undefined` to leave it alone.
 * @returns {string}
 *
 * @example Rewrites absolute specifiers and leaves relative ones alone
 * ```js
 * import { rewriteModuleImports } from "~/common/tiles.js";
 *
 * const source =
 *   `import "/a.js";\n` +
 *   `import { x } from "/b.js";\n` +
 *   `export { y } from "/c.js";\n` +
 *   `const m = import("/d.js");\n` +
 *   `import z from "./local.js";`;
 *
 * const out = rewriteModuleImports(source, (spec) => {
 *   if (spec === "/b.js") return "blob:b";
 *   if (spec === "/c.js") return "blob:c";
 *   if (spec === "/d.js") return "blob:d";
 *   return undefined;
 * });
 *
 * if (!out.includes("\"/a.js\"" )) throw new Error("unknown absolute specifier should be kept");
 * if (!out.includes("from \"blob:b\"")) throw new Error("/b.js should be rewritten");
 * if (!out.includes("from \"blob:c\"")) throw new Error("/c.js should be rewritten");
 * if (!out.includes("import(\"blob:d\")")) throw new Error("/d.js should be rewritten");
 * if (!out.includes("./local.js")) throw new Error("relative specifier should be kept");
 * ```
 */
export function rewriteModuleImports(source, resolve) {
  return source.replace(
    /(import\s*\(\s*|from\s+|import\s+)(["'`])(\/[^"'`\s]+)\2/g,
    (match, pre, quote, specifier) => {
      const url = resolve(specifier);
      return url ? `${pre}${quote}${url}${quote}` : match;
    },
  );
}

/**
 * Rewrites absolute URL specifiers in CSS `@import` statements (`@import
 * "/x.css";`, `@import url("/x.css");`, or an unquoted `url(...)`) to the
 * resolved URL. Relative/other `url()` references are left untouched. This lets
 * a tile's stylesheets import each other by path when served as Blob URLs.
 *
 * @param {string} source
 * @param {(specifier: string) => string | undefined} resolve - Maps an absolute specifier to its replacement URL, or `undefined` to leave it alone.
 * @returns {string}
 *
 * @example Rewrites absolute CSS imports and leaves relative ones alone
 * ```js
 * import { rewriteCssImports } from "~/common/tiles.js";
 *
 * const imp = "@" + "import";
 * const source =
 *   imp + ' "/base.css";\n' +
 *   imp + ' url("/theme.css");\n' +
 *   imp + " url(/vars.css);\n" +
 *   imp + ' "./local.css";\n' +
 *   'background: url("/img.png");';
 *
 * const out = rewriteCssImports(source, (spec) =>
 *   ["/base.css", "/theme.css", "/vars.css"].includes(spec) ? `blob:${spec}` : undefined,
 * );
 *
 * if (!out.includes("\"blob:/base.css\"")) throw new Error("/base.css should be rewritten");
 * if (!out.includes("url(\"blob:/theme.css\")")) throw new Error("/theme.css in url() should be rewritten");
 * if (!out.includes("url(blob:/vars.css)")) throw new Error("/vars.css unquoted url() should be rewritten");
 * if (!out.includes("./local.css")) throw new Error("relative import should be kept");
 * if (!out.includes("url(\"/img.png\")")) throw new Error("non-import url() should be kept");
 * ```
 */
export function rewriteCssImports(source, resolve) {
  return source.replace(
    /(@import\s+(?:url\(\s*)?)(["']?)(\/[^"'()\s]+)\2/g,
    (match, pre, quote, specifier) => {
      const url = resolve(specifier);
      return url ? `${pre}${quote}${url}${quote}` : match;
    },
  );
}

/**
 * Builds an inline tile facet's `resources` + `blocks` from a single piece of
 * HTML content, so the facet's index document is content addressed. The root
 * resource's CID is the CID of the raw HTML bytes.
 *
 * @param {string} html
 * @returns {{ resources: TileResources; blocks: Record<string, string> }}
 *
 * @example Produces a resources/blocks pair that resolve to the input html
 * ```js
 * import { htmlFacetTile, resolveInlineTile } from "~/common/tiles.js";
 *
 * const html = "<p>tile</p>";
 * const tile = await htmlFacetTile(html);
 * const root = await resolveInlineTile(tile.resources, tile.blocks);
 * if (root?.html !== html) throw new Error("tile should resolve back to the html");
 * ```
 */
export function htmlFacetTile(html) {
  const bytes = new TextEncoder().encode(html);
  const cid = createSyncCID(0x55, bytes);

  return {
    resources: {
      "/": {
        src: { $link: cid },
        "content-type": "text/html",
      },
    },
    blocks: encodeBlocks(new Map([[cid, bytes]])),
  };
}

/**
 * Maps a file path to a MIME content-type based on its extension.
 *
 * @param {string} path
 * @returns {string}
 *
 * @example Detects content types by extension
 * ```js
 * import { contentTypeForPath } from "~/common/tiles.js";
 *
 * if (contentTypeForPath("/") !== "text/html") throw new Error("index should be html");
 * if (contentTypeForPath("/style.css") !== "text/css") throw new Error("css should be detected");
 * if (contentTypeForPath("/app.mjs") !== "text/javascript") throw new Error("mjs should be detected");
 * ```
 */
export function contentTypeForPath(path) {
  if (path === "/" || /\.html?$/i.test(path)) return "text/html";
  if (/\.css$/i.test(path)) return "text/css";
  if (/\.m?js$/i.test(path)) return "text/javascript";
  if (/\.json$/i.test(path)) return "application/json";
  if (/\.svg$/i.test(path)) return "image/svg+xml";
  return "text/plain";
}

/**
 * Builds a tile facet's `resources` + `blocks` from a map of absolute path to
 * file content (used by the multi-file facet editor). Each file becomes a
 * content-addressed resource. `htmlFacetTile` is the single-file special case.
 *
 * @param {Record<string, string>} files - A map of absolute path (e.g. `/style.css`) to UTF-8 content.
 * @returns {{ resources: TileResources; blocks: Record<string, string> }}
 *
 * @example Builds a multi-file tile that resolves each file
 * ```js
 * import { tileFromFiles, tileResourceEntries, decodeBlocks } from "~/common/tiles.js";
 *
 * const tile = tileFromFiles({ "/": "<link rel=\"stylesheet\" href=\"/style.css\">", "/style.css": "body { color: red }" });
 * if (!tile.resources["/style.css"] || tile.resources["/style.css"]["content-type"] !== "text/css") throw new Error("css resource should be present");
 * const blocks = await decodeBlocks(tile.blocks);
 * const entries = tileResourceEntries(tile.resources, blocks);
 * if (new TextDecoder().decode(entries.get("/")?.bytes) !== "<link rel=\"stylesheet\" href=\"/style.css\">" ) throw new Error("index should round-trip");
 * ```
 */
export function tileFromFiles(files) {
  /** @type {TileResources} */
  const resources = {};
  /** @type {Map<string, Uint8Array>} */
  const blockMap = new Map();

  for (const [path, content] of Object.entries(files)) {
    if (!path.startsWith("/")) continue;
    const bytes = new TextEncoder().encode(content);
    const cid = createSyncCID(0x55, bytes);
    resources[path] = {
      src: { $link: cid },
      "content-type": contentTypeForPath(path),
    };
    blockMap.set(cid, bytes);
  }

  return { resources, blocks: encodeBlocks(blockMap) };
}

/**
 * Upgrade a stored facet's `uri` from an old `.html` bundle path to the new
 * `.tile` path. Before the tile schema the bundled facets were pointed at loose
 * `index.html` files (`diffuse://facets/…/index.html`); the build now packages
 * each facet into an `index.tile` CAR at the same directory, so a stale `.html`
 * `uri` no longer resolves. This rewrites the `diffuse://` scheme's trailing
 * `index.html` to `index.tile`. Only the Diffuse bundle's own facets moved to
 * `.tile`, so external (`http(s)://`, `at://`) and other `.html` URIs are left
 * untouched.
 *
 * @param {Record<string, unknown>} record
 * @returns {Record<string, unknown> & { uri?: string }}
 *
 * @example Rewrites a stale bundle html path to its tile
 * ```js
 * import { rewriteLegacyURIPath } from "~/common/tiles.js";
 *
 * const upgraded = rewriteLegacyURIPath({ id: "f", name: "x", uri: "diffuse://facets/data/file-manager/index.html" });
 * if (upgraded.uri !== "diffuse://facets/data/file-manager/index.tile") throw new Error("expected the tile uri");
 * const untouched = rewriteLegacyURIPath({ id: "g", name: "y", uri: "https://example.com/facet.html" });
 * if (untouched.uri !== "https://example.com/facet.html") throw new Error("external uris should be left alone");
 * ```
 */
export function rewriteLegacyURIPath(record) {
  const uri = record.uri;
  if (typeof uri !== "string") return record;

  const upgraded = uri.replace(
    /^diffuse:\/\/(.+)\/index\.html$/i,
    "diffuse://$1/index.tile",
  );
  return upgraded === uri ? record : { ...record, uri: upgraded };
}

/**
 * Lifts a legacy facet record to the current tile shape. In addition to
 * rewriting any stale `.html` bundle `uri` (see {@link rewriteLegacyURIPath}), a
 * record that carries a raw `html` string (and possibly a `cid`) is lifted into
 * a tile facet: the `html` becomes the `/` resource, with its bytes stored in
 * `blocks`, and the old `html`/`cid` fields are dropped. Already-tile records
 * (`resources` present) are left unchanged apart from any uri rewrite. This is
 * the one-shot migration for facets stored before the tile schema. Synchronous
 * because {@link htmlFacetTile} no longer needs async compression.
 *
 * @param {Record<string, unknown>} record
 * @returns {Record<string, unknown> & { resources?: TileResources; blocks?: Record<string, unknown> }}
 *
 * @example Lifts a legacy html facet into a tile
 * ```js
 * import { migrateLegacyFacet, resolveInlineTile } from "~/common/tiles.js";
 *
 * const lifted = migrateLegacyFacet({ id: "f", name: "x", html: "<p>hi</p>", cid: "old" });
 * if ("html" in lifted) throw new Error("html should be removed");
 * if ("cid" in lifted) throw new Error("cid should be removed");
 * const root = await resolveInlineTile(lifted.resources, lifted.blocks);
 * if (root?.html !== "<p>hi</p>") throw new Error("lifted content should resolve");
 * ```
 *
 * @example Rewrites a stale uri even for already-tile facets
 * ```js
 * import { migrateLegacyFacet } from "~/common/tiles.js";
 *
 * const upgraded = migrateLegacyFacet({ id: "f", name: "x", uri: "diffuse://facets/data/sources/index.html", resources: { "/": {} }, blocks: {} });
 * if (upgraded.uri !== "diffuse://facets/data/sources/index.tile") throw new Error("stale uri should be upgraded");
 * ```
 */
export function migrateLegacyFacet(record) {
  const upgraded = rewriteLegacyURIPath(record);
  const html = upgraded.html;
  if (typeof html !== "string" || upgraded.resources) return upgraded;

  const { html: _html, cid: _cid, ...rest } = upgraded;
  const tile = htmlFacetTile(html);
  return { ...rest, resources: tile.resources, blocks: tile.blocks };
}
