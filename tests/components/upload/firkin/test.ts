// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/upload/firkin", () => {
  it("reports not connected and rejects actions without a session", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/upload/firkin/element.js");
      const { SOURCE_URI } = await import("~/common/firkin/files.js");

      const upload = new mod.CLASS();
      document.body.append(upload);

      const consult = await upload.consult(SOURCE_URI);
      const file = new File([new Uint8Array([1, 2, 3])], "a.mp3", {
        type: "audio/mpeg",
      });

      let uploadError = "";
      try {
        await upload.upload({ file, uri: SOURCE_URI });
      } catch (err) {
        uploadError = err instanceof Error ? err.message : String(err);
      }

      let deleteError = "";
      try {
        await upload.delete("firkin://me/files/a.mp3");
      } catch (err) {
        deleteError = err instanceof Error ? err.message : String(err);
      }

      let createError = "";
      try {
        await upload.createSource({});
      } catch (err) {
        createError = err instanceof Error ? err.message : String(err);
      }

      return {
        name: mod.NAME,
        scheme: upload.SCHEME,
        consult,
        uploadError,
        deleteError,
        createError,
      };
    });

    expect(result.name).toBe("du-firkin");
    expect(result.scheme).toBe("firkin");
    expect(result.consult).toEqual({ supported: true, consult: "no" });
    expect(result.uploadError).toContain("Firkin is not connected");
    expect(result.deleteError).toContain("Firkin is not connected");
    expect(result.createError).toContain("Firkin does not use OAuth");
  });
});
