import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/upload/webdav", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import("~/components/upload/webdav/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return upload.SCHEME;
    });

    expect(scheme).toBe("webdav");
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/upload/webdav/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return await upload.consult("webdav");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("consult returns undetermined for an invalid WebDAV URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/upload/webdav/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return await upload.consult("webdav://");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("does not define authorize (WebDAV has no OAuth flow)", async () => {
    const hasAuthorize = await testWeb(async () => {
      const mod = await import("~/components/upload/webdav/element.js");
      const upload = new mod.CLASS();
      document.body.append(upload);
      return "authorize" in upload;
    });

    expect(hasAuthorize).toBe(false);
  });
});
