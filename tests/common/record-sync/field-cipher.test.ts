// @ts-nocheck -- the prototype is plain JS and the tests build records directly

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";
import { base64url } from "iso-base/rfc4648";

import {
  fieldContext,
  importFieldKey,
  openField,
  sealField,
} from "~/common/record-sync/field-cipher.js";
import {
  changeDocument,
  createDocument,
  encodeDocument,
  headsOf,
} from "~/components/output/raw/firkin/encrypted/incremental-loro/doc.js";

const RAW_KEY = new Uint8Array(32).map((_, index) => index);
const IV = new Uint8Array(12).map((_, index) => index);
const PLAINTEXT = "https://example.com/t1.mp3";

/** The vector from specs/record-sync/SPEC.md. */
const VECTOR =
  "g0eiAQMEYmsxoQVMAAECAwQFBgcICQoLWCovdqJrtt_tNOg59ubBhR1D4LnqG4RKcRFIVMc5ekcDZJhGrl6CGs4yutE";

describe("common/record-sync/field-cipher", () => {
  it("matches the spec vector", async () => {
    const key = await importFieldKey(RAW_KEY);
    const sealed = await sealField(PLAINTEXT, {
      key,
      context: fieldContext("tracks", "t1", "uri"),
      keyId: "k1",
      iv: IV,
    });

    expect(sealed).toBe(VECTOR);

    const hex = [...base64url.decode(sealed)]
      .map((byte) => byte.toString(16).padStart(2, "0"))
      .join("");

    expect(hex).toBe(
      "8347a2010304626b31a1054c000102030405060708090a0b582a2f76a26bb6dfed34e839f6e6c1851d43e0b9ea1b844a71114854c7397a4703649846ae5e821ace32bad1",
    );
  });

  it("round-trips a value", async () => {
    const key = await importFieldKey(RAW_KEY);
    const context = fieldContext("tracks", "t1", "uri");
    const sealed = await sealField(PLAINTEXT, { key, context, iv: IV });

    expect(await openField(sealed, { key, context })).toBe(PLAINTEXT);
  });

  it("binds the value to its collection, record and field", async () => {
    const key = await importFieldKey(RAW_KEY);
    const sealed = await sealField(PLAINTEXT, {
      key,
      context: fieldContext("tracks", "t1", "uri"),
      iv: IV,
    });

    for (
      const context of [
        fieldContext("tracks", "t2", "uri"),
        fieldContext("tracks", "t1", "image"),
        fieldContext("facets", "t1", "uri"),
      ]
    ) {
      let failed = false;

      try {
        await openField(sealed, { key, context });
      } catch {
        failed = true;
      }

      expect(failed).toBe(true);
    }

    expect(
      await openField(sealed, {
        key,
        context: fieldContext("tracks", "t1", "uri"),
      }),
    ).toBe(PLAINTEXT);
  });

  it("rejects a tampered ciphertext", async () => {
    const key = await importFieldKey(RAW_KEY);
    const context = fieldContext("tracks", "t1", "uri");
    const sealed = await sealField(PLAINTEXT, { key, context, iv: IV });

    const bytes = base64url.decode(sealed);
    bytes[bytes.length - 1] ^= 1;

    let failed = false;

    try {
      await openField(base64url.encode(bytes), { key, context });
    } catch {
      failed = true;
    }

    expect(failed).toBe(true);
  });

  it("keeps an unchanged value's ciphertext", async () => {
    const key = await importFieldKey(RAW_KEY);
    const context = fieldContext("tracks", "t1", "uri");
    const sealed = await sealField(PLAINTEXT, { key, context, iv: IV });

    // A random nonce would produce different bytes for the same plaintext.
    const fresh = await sealField(PLAINTEXT, { key, context });
    expect(fresh).not.toBe(sealed);

    // Reusing the previous value keeps the record stable.
    const reused = await sealField(PLAINTEXT, {
      key,
      context,
      previous: sealed,
    });
    expect(reused).toBe(sealed);

    const changed = await sealField(`${PLAINTEXT}?v=2`, {
      key,
      context,
      previous: sealed,
    });
    expect(changed).not.toBe(sealed);
  });

  it("does not churn the document when other fields change", async () => {
    const key = await importFieldKey(RAW_KEY);
    const context = fieldContext("tracks", "t1", "uri");
    const sealed = await sealField(PLAINTEXT, { key, context, iv: IV });

    const record = (title) => ({
      $type: "sh.diffuse.output.track",
      id: "t1",
      title,
      $enc: { uri: sealed },
    });

    const doc = createDocument("aa".repeat(16));
    expect(changeDocument(doc, [record("Song")]).changed).toBe(true);

    const heads = headsOf(doc).join(";");

    // Editing an unrelated field with the reused ciphertext only changes that
    // field.
    expect(changeDocument(doc, [record("Renamed")]).changed).toBe(true);
    expect(headsOf(doc).join(";")).not.toBe(heads);

    const renamedHeads = headsOf(doc).join(";");
    const size = encodeDocument(doc).length;

    // Re-saving the same record is a no-op.
    expect(changeDocument(doc, [record("Renamed")]).changed).toBe(false);
    expect(headsOf(doc).join(";")).toBe(renamedHeads);
    expect(encodeDocument(doc).length).toBe(size);

    // Re-sealing the unchanged URI rewrites the record and grows the document.
    const repacked = await sealField(PLAINTEXT, { key, context });
    expect(
      changeDocument(doc, [{
        $type: "sh.diffuse.output.track",
        id: "t1",
        title: "Renamed",
        $enc: { uri: repacked },
      }]).changed,
    ).toBe(true);
    expect(headsOf(doc).join(";")).not.toBe(renamedHeads);
    expect(encodeDocument(doc).length).toBeGreaterThan(size);
  });
});
