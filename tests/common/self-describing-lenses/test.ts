import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  wrap,
  unwrap,
  isSelfDescribing,
  collectionSchema,
} from "~/common/self-describing.js";
import {
  project,
  migrateEnvelope,
  encodeCollection,
  encodeJsonCollection,
  decodeJsonCollection,
  saveJsonCollection,
  readStoredEnvelope,
  writeBack,
} from "~/common/lens.js";
import {
  register,
  resolve,
} from "~/common/lens-registry.js";
import { resolveInlineTile, rewriteLegacyURIPath } from "~/common/tiles.js";

// Unit tests for the self-describing envelope + migration/write-back machinery.
// These use distinct NSIDs per test so the module-level lens registry does not
// collide across cases.
describe("common/self-describing + lens", () => {
  describe("self-describing envelope", () => {
    it("wraps data with the collection's NSID", () => {
      const env = encodeCollection([{ id: "a" }], "facets");
      expect(env.$schema).toBe("sh.diffuse.output.facet");
      expect(isSelfDescribing(env)).toBe(true);
    });

    it("unwrap tolerates a legacy bare array", () => {
      const out = unwrap([{ id: "a" }], { $schema: "sh.diffuse.output.facet" });
      expect(out.envelope).toBeNull();
      const rec = (out.data as Array<{ id: string }>)[0];
      expect(rec.id).toBe("a");
    });

    it("collectionSchema returns the current NSID", () => {
      expect(collectionSchema("tracks")).toBe("sh.diffuse.output.track");
    });
  });

  describe("migration", () => {
    it("migrates a stale envelope to the current NSID", () => {
      register({
        id: "f-old-current",
        source: "sh.diffuse.output.facetOld",
        target: "sh.diffuse.output.facet",
        steps: [{ rename_field: { old: "favourite", new: "starred" } }],
      });
      const envelope = wrap(
        [{ id: "a", favourite: true }],
        { schema: "sh.diffuse.output.facetOld" },
      );
      const out = migrateEnvelope(envelope, "facets", resolve);
      const rec = out.data[0] as Record<string, unknown>;
      expect(rec.starred).toBe(true);
      expect(rec.favourite).toBeUndefined();
      expect(out.envelope?.$schema).toBe("sh.diffuse.output.facet");
      expect(out.envelope?.$schemaHistory).toHaveLength(1);
    });

    it("does not migrate when the envelope NSID already matches", () => {
      const envelope = wrap([{ id: "a" }], { schema: "sh.diffuse.output.facet" });
      const out = migrateEnvelope(envelope, "facets", resolve);
      const rec = (out.data as Array<{ id: string }>)[0];
      expect(rec.id).toBe("a");
      expect(out.envelope?.$schemaHistory).toHaveLength(0);
    });

    it("lifts a legacy html facet into a tile on read", async () => {
      // A facet stored before the tile schema: a raw `html` string, no `resources`.
      const stored = encodeJsonCollection([
        { id: "f1", name: "Legacy", html: "<p>hello</p>", cid: "old-cid" },
      ], "facets");

      // The encoder read path (decodeJsonCollection -> migrateEnvelope) lifts it.
      const out = decodeJsonCollection(stored, "facets") as Array<Record<string, unknown>>;
      const rec = out[0];
      expect(rec.id).toBe("f1");
      expect(rec.name).toBe("Legacy");
      expect(rec.html).toBeUndefined();
      expect(rec.cid).toBeUndefined();
      expect(typeof rec.resources).toBe("object");
      expect(typeof rec.blocks).toBe("object");

      // The lifted tile resolves back to the original HTML.
      const root = await resolveInlineTile(
        rec.resources as Record<string, { src: unknown; "content-type"?: string }>,
        rec.blocks as Record<string, string>,
      );
      expect(root?.html).toBe("<p>hello</p>");
    });

    it("migrateEnvelope lifts legacy facets and keeps already-tile facets untouched", () => {
      const envelope = wrap([
        { id: "a", name: "x", html: "<p>hi</p>" },
        { id: "b", name: "y", resources: { "/": { src: { $link: "bafk..." } } }, blocks: {} },
      ], { schema: "sh.diffuse.output.facet" });
      const out = migrateEnvelope(envelope, "facets", resolve);
      const records = out.data as Array<Record<string, unknown>>;

      const lifted = records.find((r) => r.id === "a")!;
      expect(lifted.html).toBeUndefined();
      expect(typeof lifted.resources).toBe("object");
      expect(typeof lifted.blocks).toBe("object");

      const alreadyTile = records.find((r) => r.id === "b")!;
      expect(alreadyTile.resources).toBeDefined();
      expect(alreadyTile.blocks).toBeDefined();
    });

    it("lifts a legacy facet uri from the old .html bundle path to .tile on read", () => {
      // A facet stored before the tile conversion points at the loose bundle
      // `index.html`; the build now serves an `index.tile` CAR at the same dir.
      const envelope = wrap([
        { id: "f1", name: "Old", uri: "diffuse://facets/data/file-manager/index.html" },
        // Already-tile facets also carry a stale uri and must be upgraded.
        { id: "f2", name: "Tile", uri: "diffuse://facets/data/sources/index.html", resources: { "/": { src: { $link: "bafk..." } } }, blocks: {} },
        // External uris are not bundle paths and must be left alone.
        { id: "f3", name: "Ext", uri: "https://example.com/facet.html" },
      ], { schema: "sh.diffuse.output.facet" });
      const out = migrateEnvelope(envelope, "facets", resolve);
      const records = out.data as Array<Record<string, unknown>>;

      const lifted = records.find((r) => r.id === "f1")!;
      expect(lifted.uri).toBe("diffuse://facets/data/file-manager/index.tile");

      const tile = records.find((r) => r.id === "f2")!;
      expect(tile.uri).toBe("diffuse://facets/data/sources/index.tile");
      expect(tile.resources).toBeDefined();

      const ext = records.find((r) => r.id === "f3")!;
      expect(ext.uri).toBe("https://example.com/facet.html");
    });

    it("rewriteLegacyURIPath rewrites only stale diffuse bundle paths", () => {
      const upgraded = rewriteLegacyURIPath({
        id: "f", name: "x", uri: "diffuse://facets/themes/blur/facet/index.html",
      });
      expect(upgraded.uri).toBe("diffuse://facets/themes/blur/facet/index.tile");

      const untouched = rewriteLegacyURIPath({
        id: "g", name: "y", uri: "https://example.com/facet.html",
      });
      expect(untouched.uri).toBe("https://example.com/facet.html");

      // A missing/untyped uri passes through as-is.
      const noUri = rewriteLegacyURIPath({ id: "h", name: "z" });
      expect("uri" in noUri).toBe(false);
    });

    it("migrateEnvelope tolerates non-array facets data (no crash, passes through)", () => {
      // A malformed/legacy stored facets value that is not an array must not
      // throw (the lift would otherwise call .map on a non-function).
      const envelope = {
        $schema: "sh.diffuse.output.facet",
        $schemaHistory: [],
        data: { id: "a", name: "x", html: "<p>hi</p>" },
      };
      let out;
      expect(() => {
        out = migrateEnvelope(envelope as never, "facets", resolve);
      }).not.toThrow();
      expect(out).toBeDefined();
    });
  });

  describe("lens projection", () => {
    it("renames a field via a lens document", () => {
      const out = project(
        [{ $type: "sh.diffuse.output.facet", id: "a", favourite: true }],
        {
          id: "f",
          source: "sh.diffuse.output.facet",
          target: "sh.diffuse.output.facet2",
          steps: [{ rename_field: { old: "favourite", new: "starred" } }],
        },
      );
      const rec = (out as Array<Record<string, unknown>>)[0];
      expect(rec.starred).toBe(true);
      expect(rec.favourite).toBeUndefined();
    });
  });

  describe("JSON encode/decode + save wiring", () => {
    it("round-trips through encode/decode", () => {
      const bytes = encodeJsonCollection([{ id: "a" }], "tracks", true);
      const out = decodeJsonCollection(bytes, "tracks") as Array<{ id: string }>;
      expect(out[0].id).toBe("a");
    });

    it("saveJsonCollection wires the save path (guarded no-op)", async () => {
      const out = await saveJsonCollection([{ id: "a" }], "tracks", null);
      const back = decodeJsonCollection(out, "tracks") as Array<{ id: string }>;
      expect(back[0].id).toBe("a");
    });

    it("decodeJsonCollection accepts an already-parsed envelope object", () => {
      const env = encodeCollection([{ id: "a" }], "tracks");
      const out = decodeJsonCollection(env, "tracks") as Array<{ id: string }>;
      expect(out[0].id).toBe("a");
    });

    it("decodeJsonCollection returns an array even for a non-array stored value", () => {
      const out = decodeJsonCollection(
        { $schema: "sh.diffuse.output.track", $schemaHistory: [], data: { id: "a" } },
        "tracks",
      );
      expect(Array.isArray(out)).toBe(true);
    });

    it("readStoredEnvelope returns the stored envelope", () => {
      const stored = encodeJsonCollection([{ id: "a" }], "tracks", true);
      const env = readStoredEnvelope(stored, "tracks");
      expect(env?.$schema).toBe("sh.diffuse.output.track");
    });
  });

  describe("writeBack", () => {
    it("uses pure-JS projection when no complement is present", async () => {
      const out = await writeBack(
        { $type: "sh.diffuse.output.facet", id: "a", favourite: true },
        {
          lens: {
            id: "f",
            source: "sh.diffuse.output.facet",
            target: "sh.diffuse.output.facet2",
            steps: [{ rename_field: { old: "favourite", new: "starred" } }],
          },
        },
      );
      const rec = out as Record<string, unknown>;
      expect(rec.starred).toBe(true);
      expect(rec.favourite).toBeUndefined();
    });
  });
});