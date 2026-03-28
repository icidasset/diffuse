import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/input/opensubsonic", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import("~/components/input/opensubsonic/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return input.SCHEME;
    });

    expect(scheme).toBe("opensubsonic");
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/opensubsonic/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("opensubsonic");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("consult returns undetermined for a full URI (defers to network check)", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/opensubsonic/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult(
        "opensubsonic://user:pass@localhost:4040?tls=0",
      );
    });

    // consultServerCached will fail to reach the server and return false
    expect(result.supported).toBe(true);
  });

  it("detach with opensubsonic scheme removes all opensubsonic tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/opensubsonic/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "opensubsonic://user:pass@subsonic.example.com?tls=t",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "opensubsonic://user:pass@other.example.com?tls=t",
        },
      ];

      return await input.detach({ fileUriOrScheme: "opensubsonic", tracks });
    });

    expect(remaining.length).toBe(0);
  });

  it("detach with non-matching scheme returns all tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/opensubsonic/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "opensubsonic://user:pass@subsonic.example.com?tls=t",
        },
      ];

      return await input.detach({ fileUriOrScheme: "https", tracks });
    });

    expect(remaining.length).toBe(1);
  });

  it("detach with specific server URI removes only matching tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/opensubsonic/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "opensubsonic://user:pass@server-a.example.com?tls=t",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "opensubsonic://user:pass@server-b.example.com?tls=t",
        },
      ];

      return await input.detach({
        fileUriOrScheme:
          "opensubsonic://user:pass@server-a.example.com?tls=t",
        tracks,
      });
    });

    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });
});
