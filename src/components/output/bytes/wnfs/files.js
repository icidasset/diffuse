import { Path } from "@wnfs-wg/nest";
import { bundle, bundleId } from "~/common/bundles.js";
import { decodeJsonCollection, encodeCollection } from "~/common/lens.js";
import { hash } from "~/common/utils.js";

import { WNFS_OUTPUT_DIR } from "./constants.js";

/**
 * @import {FileSystem} from "@wnfs-wg/nest"
 * @import {BundleSelector} from "~/common/bundles.d.ts"
 * @import {CollectionName} from "~/common/self-describing.js"
 */

/**
 * Per-collection map of bundle id → content hash.
 *
 * WNFS encrypts every write with a fresh nonce, so rewriting an unchanged
 * bundle would still produce a new CID and upload it. The manifest lets a save
 * skip bundles whose contents (including their place in the collection) did
 * not change.
 */
const MANIFEST = "__manifest";

/**
 * Read a collection from the WNFS output directory.
 *
 * Bundled collections live in a directory with one file per bundle; other
 * collections (and data written by the previous single-file layout) live in
 * one file named after the collection. A directory is preferred, so a legacy
 * file is still read until the next write migrates it.
 *
 * @param {FileSystem} fs
 * @param {CollectionName} name
 * @returns {Promise<any[]>}
 */
export async function readCollection(fs, name) {
  const path = Path.priv(WNFS_OUTPUT_DIR, name);

  /** @type {{ name: string }[]} */
  let entries;

  try {
    entries = await fs.ls(path);
  } catch {
    // Not a directory: either a single legacy file or nothing at all.
    try {
      return decodeJsonCollection(await fs.read(path, "bytes"), name);
    } catch {
      return [];
    }
  }

  /** @type {{ value: any; position: number }[]} */
  const items = [];

  for (const entry of entries) {
    if (entry.name === MANIFEST) continue;

    try {
      const bytes = await fs.read(
        Path.priv(WNFS_OUTPUT_DIR, name, entry.name),
        "bytes",
      );
      const parsed = JSON.parse(new TextDecoder().decode(bytes));
      const values = decodeJsonCollection(parsed, name);
      const positions = Array.isArray(parsed.index) ? parsed.index : undefined;

      values.forEach((value, i) => {
        const position = positions?.[i];
        items.push({
          value,
          position: typeof position === "number" ? position : Infinity,
        });
      });
    } catch (err) {
      console.warn(`Could not read the '${name}/${entry.name}' bundle:`, err);
    }
  }

  // Restore the order of the original collection. Bundles written by this
  // output carry each item's global position; legacy files are already
  // ordered, so `Infinity` preserves their file order (sort is stable).
  items.sort((a, b) => {
    if (a.position === b.position) return 0;
    return a.position < b.position ? -1 : 1;
  });

  return items.map((item) => item.value);
}

/**
 * Write a collection to the WNFS output directory.
 *
 * With a `selector`, the collection is split into one file per bundle (and
 * stale bundles are removed). Without one, the collection is written as a
 * single file, replacing a bundle directory if the collection used to be
 * bundled.
 *
 * Each bundle file is a normal self-describing collection envelope plus an
 * `index` array holding the global position of every item, so reading the
 * bundles back preserves the collection's order. A small `__manifest` file
 * records each bundle's content hash so a save only rewrites the bundles that
 * actually changed (WNFS re-encrypts every write with a fresh nonce, so an
 * unchanged file would otherwise get a new CID and upload again).
 *
 * @param {FileSystem} fs
 * @param {CollectionName} name
 * @param {any[]} data
 * @param {BundleSelector<any>} [selector]
 * @returns {Promise<void>}
 */
export async function writeCollection(fs, name, data, selector) {
  const items = Array.isArray(data) ? data : [];
  const path = Path.priv(WNFS_OUTPUT_DIR, name);

  if (!selector) {
    // A collection can switch layouts (for example a custom build that stops
    // bundling it); remove the old directory before writing the file.
    try {
      await fs.ls(path);
      await fs.remove(path);
    } catch {
      // Not a directory.
    }

    const bytes = new TextEncoder().encode(
      JSON.stringify(encodeCollection(items, name)),
    );

    await fs.write(path, "bytes", bytes);
    return;
  }

  await ensureDirectory(fs, path);

  const bundles = bundle(items, { by: selector, id: bundleId });

  /** @type {Set<string>} */
  let existing;
  /** @type {Record<string, string>} */
  let previous = {};

  try {
    existing = new Set((await fs.ls(path)).map((entry) => entry.name));

    const raw = await fs.read(
      Path.priv(WNFS_OUTPUT_DIR, name, MANIFEST),
      "bytes",
    );
    const parsed = JSON.parse(new TextDecoder().decode(raw));

    if (parsed && typeof parsed === "object" && !Array.isArray(parsed)) {
      previous = parsed;
    }
  } catch {
    // No manifest yet (first write, or the legacy single-file layout).
    existing ??= new Set();
  }

  const positions = new Map();
  items.forEach((item, index) => {
    if (typeof item?.id === "string") positions.set(item.id, index);
  });

  const next = bundles.map((entry) => {
    const envelope = {
      ...encodeCollection(entry.items, name),
      index: entry.items.map((value) =>
        typeof value?.id === "string"
          ? (positions.get(value.id) ?? -1)
          : -1
      ),
    };

    return { bundle: entry, envelope, hash: hash(envelope) };
  });

  const keep = new Set(next.map((entry) => entry.bundle.id));

  // Only write bundles whose content changed (or whose file disappeared) —
  // re-writing an unchanged one would still cost a new encrypted block.
  for (const entry of next) {
    if (previous[entry.bundle.id] === entry.hash && existing.has(entry.bundle.id)) {
      continue;
    }

    const bytes = new TextEncoder().encode(JSON.stringify(entry.envelope));
    await fs.write(
      Path.priv(WNFS_OUTPUT_DIR, name, entry.bundle.id),
      "bytes",
      bytes,
    );
  }

  for (const entry of existing) {
    if (entry === MANIFEST || keep.has(entry)) continue;

    try {
      await fs.remove(Path.priv(WNFS_OUTPUT_DIR, name, entry));
    } catch {
      // The entry disappeared between listing and removing it.
    }
  }

  /** @type {Record<string, string>} */
  const manifest = Object.fromEntries(
    next.map((entry) => [entry.bundle.id, entry.hash]),
  );

  if (!sameManifest(previous, manifest)) {
    await fs.write(
      Path.priv(WNFS_OUTPUT_DIR, name, MANIFEST),
      "bytes",
      new TextEncoder().encode(JSON.stringify(manifest)),
    );
  }
}

/**
 * Make sure `path` is a directory, replacing a legacy file of the same name.
 *
 * @param {FileSystem} fs
 * @param {any} path
 */
async function ensureDirectory(fs, path) {
  try {
    await fs.ls(path);
    return;
  } catch {
    // Not a directory (missing, or a legacy file with the collection's name).
  }

  try {
    await fs.remove(path);
  } catch {
    // Missing.
  }

  await fs.ensureDirectory(path);
}

/**
 * @param {Record<string, string>} a
 * @param {Record<string, string>} b
 */
function sameManifest(a, b) {
  const aKeys = Object.keys(a);
  const bKeys = Object.keys(b);
  if (aKeys.length !== bKeys.length) return false;

  return aKeys.every((key) => a[key] === b[key]);
}
