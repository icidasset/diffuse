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