import * as CBOR from "@atcute/cbor";
import * as CID from "@atcute/cid";
import { toSha256 } from "@atcute/uint8array";

/**
 * Build-time helper for creating DASL "Tiles in CAR" archives.
 *
 * A tile CAR is a CARv1 archive whose header block is the MASL metadata (plus
 * the `roots` and `version` fields CAR requires) and which contains every
 * resource as a block, keyed by its CID. The `/` resource is the tile's index
 * document. See https://dasl.ing/tiles.html ("Tiles in CAR") and
 * https://dasl.ing/car.html.
 *
 * Relative (`./…`) URLs are deliberately NOT included as resources: they refer
 * to shared assets served by the Diffuse build (resolved by the loader against
 * the build root), which stay external.
 */

export interface TileFile {
  /** The file's content (UTF-8). */
  content: string;
  /** Optional MIME type; inferred from the path's extension when omitted. */
  contentType?: string;
}

/**
 * @param {string} path
 * @returns {string}
 */
function contentTypeForPath(path: string): string {
  if (path === "/" || /\.html?$/i.test(path)) return "text/html";
  if (/\.css$/i.test(path)) return "text/css";
  if (/\.m?js$/i.test(path)) return "text/javascript";
  if (/\.json$/i.test(path)) return "application/json";
  if (/\.svg$/i.test(path)) return "image/svg+xml";
  return "text/plain";
}

/** LEB128 unsigned varint. */
function varint(n: number): Uint8Array {
  const out: number[] = [];
  do {
    let byte = n & 0x7f;
    n >>>= 7;
    if (n !== 0) byte |= 0x80;
    out.push(byte);
  } while (n !== 0);
  return new Uint8Array(out);
}

function concat(...parts: Uint8Array[]): Uint8Array {
  const length = parts.reduce((n, p) => n + p.length, 0);
  const out = new Uint8Array(length);
  let offset = 0;
  for (const part of parts) {
    out.set(part, offset);
    offset += part.length;
  }
  return out;
}

/**
 * Builds a tile CAR from a map of absolute path to file content. The CAR header
 * is the MASL manifest (rooted at `/`) plus `version`/`roots`; every resource is
 * a block keyed by CID. Returns the raw CAR bytes (a `.tile` file).
 *
 * @param {Record<string, TileFile>} files
 * @param {{ name?: string; description?: string }} [meta]
 * @returns {Promise<Uint8Array>}
 */
export async function buildTileCar(
  files: Record<string, TileFile>,
  meta: { name?: string; description?: string } = {},
): Promise<Uint8Array> {
  const encoder = new TextEncoder();

  // 1. Compute each resource's CID and build the MASL `resources` map.
  const resources: Record<string, unknown> = {};
  const blocks: { cid: Uint8Array; data: Uint8Array }[] = [];
  for (const [path, file] of Object.entries(files)) {
    if (!path.startsWith("/")) continue;
    const data = encoder.encode(file.content);
    const cid = await CID.create(0x55, data);
    resources[path] = {
      src: { $link: CID.toString(cid) },
      "content-type": file.contentType ?? contentTypeForPath(path),
    };
    blocks.push({ cid: cid.bytes, data });
  }

  // 2. The root is a content address of the manifest (without the CAR-only
  //    `roots`/`version` fields, which would be circular).
  const manifest = {
    ...(meta.name ? { name: meta.name } : {}),
    ...(meta.description ? { description: meta.description } : {}),
    resources,
  };
  const manifestBytes = CBOR.encode(manifest);
  const manifestDigest = await toSha256(manifestBytes);
  const root = CID.fromDigest(0x71, manifestDigest);

  // Header: the MASL manifest + the CAR-required fields.
  const headerData = CBOR.encode({
    ...manifest,
    version: 1,
    roots: [CBOR.toCidLink(root)],
  });
  const headerBytes = concat(varint(headerData.length), headerData);

  // 3. Entries.
  const entryBytes: Uint8Array[] = [];
  for (const block of blocks) {
    const entryLength = block.cid.length + block.data.length;
    entryBytes.push(concat(varint(entryLength), block.cid, block.data));
  }

  return concat(headerBytes, ...entryBytes);
}