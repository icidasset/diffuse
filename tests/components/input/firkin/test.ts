// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/input/firkin", () => {
  it("exposes the Firkin source", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/firkin/element.js");
      const { SCHEME, SOURCE_URI } = await import(
        "~/common/firkin/files.js"
      );

      const input = new mod.CLASS();
      document.body.append(input);

      return {
        name: mod.NAME,
        scheme: input.SCHEME,
        expectedScheme: SCHEME,
        sourceUri: SOURCE_URI,
        sources: input.sources([]),
      };
    });

    expect(result).toEqual({
      name: "di-firkin",
      scheme: "firkin",
      expectedScheme: "firkin",
      sourceUri: "firkin://me",
      sources: [{ label: "Firkin", uri: "firkin://me" }],
    });
  });

  it("reports not connected and keeps cached tracks without a session", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/firkin/element.js");
      const { SOURCE_URI } = await import("~/common/firkin/files.js");

      const input = new mod.CLASS();
      document.body.append(input);

      const consult = await input.consult(SOURCE_URI);
      const groupConsult = await input.groupConsult([SOURCE_URI]);
      const artwork = await input.artwork(SOURCE_URI);

      const tracks = await input.list([
        {
          $type: "sh.diffuse.output.track",
          id: "keep-me",
          uri: SOURCE_URI,
          kind: "placeholder",
        },
      ]);

      const detached = await input.detach({
        tracks: [
          { uri: "firkin://me/files/a.mp3" },
          { uri: "https://example.com/b.mp3" },
        ],
      });

      return {
        consult,
        groupConsult,
        artwork,
        tracks: tracks.map((track) => track.id),
        detached: detached.map((track) => track.uri),
      };
    });

    expect(result.consult).toEqual({ supported: true, consult: "no" });
    expect(result.groupConsult).toEqual({
      firkin: {
        available: "no",
        reason: "Firkin is not connected",
        scheme: "firkin",
        uris: ["firkin://me"],
      },
    });
    expect(result.artwork).toBe(null);
    expect(result.tracks).toEqual(["keep-me"]);
    expect(result.detached).toEqual(["https://example.com/b.mp3"]);
  });
});
