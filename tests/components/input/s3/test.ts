import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/input/s3", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return input.SCHEME;
    });

    expect(scheme).toBe("s3");
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("s3");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("consult returns undetermined for an invalid S3 URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("s3://notvalid");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("detach with s3 scheme removes all s3 tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "s3://key:secret@s3.amazonaws.com/music/track1.mp3?bucketName=my-bucket&region=us-east-1",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "s3://key:secret@s3.amazonaws.com/music/track2.mp3?bucketName=my-bucket&region=us-east-1",
        },
      ];

      return await input.detach({ fileUriOrScheme: "s3", tracks });
    });

    expect(remaining.length).toBe(0);
  });

  it("detach with non-matching scheme returns all tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "s3://key:secret@s3.amazonaws.com/music/track1.mp3?bucketName=my-bucket&region=us-east-1",
        },
      ];

      return await input.detach({ fileUriOrScheme: "https", tracks });
    });

    expect(remaining.length).toBe(1);
  });

  it("detach with specific bucket URI removes only matching tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      // bucketId is keyed by accessKey:secretKey@host — use different hosts
      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "s3://key:secret@s3-host-a.amazonaws.com/track1.mp3?bucketName=bucket-a&region=us-east-1",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "s3://key:secret@s3-host-b.amazonaws.com/track2.mp3?bucketName=bucket-b&region=us-east-1",
        },
      ];

      return await input.detach({
        fileUriOrScheme:
          "s3://key:secret@s3-host-a.amazonaws.com/?bucketName=bucket-a&region=us-east-1",
        tracks,
      });
    });

    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });

  it("artwork returns null", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.artwork(
        "s3://key:secret@s3.amazonaws.com/music/track1.mp3?bucketName=my-bucket&region=us-east-1",
      );
    });

    expect(result).toBe(null);
  });

  it("sources returns labels with bucket name and host", async () => {
    const sources = await testWeb(async () => {
      const mod = await import("~/components/input/s3/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "s3://key:secret@s3.amazonaws.com/music/track1.mp3?bucketName=my-bucket&region=us-east-1",
        },
      ];

      return input.sources(tracks);
    });

    expect(sources.length).toBe(1);
    expect(sources[0].label).toContain("my-bucket");
  });
});
