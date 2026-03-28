import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/s3", () => {
  it("ready returns false when no bucket is configured", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.ready();
    });

    expect(result).toBe(false);
  });

  it("bucket returns undefined initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.bucket() ?? null;
    });

    expect(result).toBe(null);
  });

  it("getBucket returns undefined when no bucket is stored", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return (await output.getBucket()) ?? null;
    });

    expect(result).toBe(null);
  });

  it("setBucket updates the bucket signal", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.setBucket({
        accessKey: "myKey",
        secretKey: "mySecret",
        host: "s3.amazonaws.com",
        bucketName: "my-bucket",
        path: "/",
        region: "us-east-1",
      });

      return output.bucket();
    });

    expect(result?.bucketName).toBe("my-bucket");
    expect(result?.accessKey).toBe("myKey");
  });

  it("ready returns true after setBucket when online", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.setBucket({
        accessKey: "myKey",
        secretKey: "mySecret",
        host: "s3.amazonaws.com",
        bucketName: "my-bucket",
        path: "/",
        region: "us-east-1",
      });

      // ready = bucket !== undefined && isOnline
      // isOnline follows navigator.onLine which is true in headless Chromium
      return output.ready();
    });

    expect(result).toBe(true);
  });

  it("unsetBucket clears the bucket signal", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.setBucket({
        accessKey: "myKey",
        secretKey: "mySecret",
        host: "s3.amazonaws.com",
        bucketName: "my-bucket",
        path: "/",
        region: "us-east-1",
      });

      await output.unsetBucket();
      return output.bucket() ?? null;
    });

    expect(result).toBe(null);
  });

  it("ready returns false after unsetBucket", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/s3/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.setBucket({
        accessKey: "myKey",
        secretKey: "mySecret",
        host: "s3.amazonaws.com",
        bucketName: "my-bucket",
        path: "/",
        region: "us-east-1",
      });

      await output.unsetBucket();
      return output.ready();
    });

    expect(result).toBe(false);
  });
});
