import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/upload/s3", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import("~/components/upload/s3/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return upload.SCHEME;
    });

    expect(scheme).toBe("s3");
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/upload/s3/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return await upload.consult("s3");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("consult returns undetermined for an invalid S3 URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/upload/s3/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return await upload.consult("s3://notvalid");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("does not define authorize (S3 has no OAuth flow)", async () => {
    const hasAuthorize = await testWeb(async () => {
      const mod = await import("~/components/upload/s3/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return "authorize" in upload;
    });

    expect(hasAuthorize).toBe(false);
  });
});
