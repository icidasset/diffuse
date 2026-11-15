// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/input/firkin", () => {
  it("resolves generic blobs via RASL without a session", async () => {
    const result = await testWeb(async () => {
      const { cidForBytes } = await import("~/vendor/firkin/index.js");
      const { buildBlobURI, parseBlobURI } = await import(
        "~/common/firkin/files.js"
      );
      const mod = await import("~/components/input/firkin/element.js");

      const cid = cidForBytes(new Uint8Array([1]), { codec: 0x55 });
      const uri = buildBlobURI({
        host: "firkin.example",
        cid,
        name: "a.mp3",
      });
      const authedUri = buildBlobURI({
        host: "firkin.example",
        cid,
        token: "ucan-token",
      });

      const input = new mod.CLASS();
      // Bypass any service-worker-cached worker bundle from an earlier test
      // run; the query is ignored by the worker itself.
      input.workerQuery = () => ({ v: String(Date.now()) });
      document.body.append(input);

      const consult = await input.consult(uri);
      const groupConsult = await input.groupConsult([uri]);
      const resolved = await input.resolve({ uri });
      const authedResolved = await input.resolve({ uri: authedUri });
      const listed = await input.list([
        {
          $type: "sh.diffuse.output.track",
          id: "generic-1",
          uri,
        },
      ]);
      const sources = input.sources([{ uri }]);

      return {
        uri,
        authedUri,
        cid,
        cidType: typeof cid,
        parsed: parseBlobURI(uri),
        authedParsed: parseBlobURI(authedUri),
        consult,
        groupConsult,
        resolved,
        authedResolved,
        listed: listed.map((track) => track.uri),
        sources,
      };
    });

    expect(result.uri).toBe(
      `firkin://firkin.example/blob/${result.cid}?name=a.mp3`,
    );
    expect(result.parsed).toEqual({
      host: "firkin.example",
      cid: result.cid,
      name: "a.mp3",
      token: undefined,
    });
    expect(result.consult).toEqual({ supported: true, consult: "yes" });
    expect(result.groupConsult).toEqual({
      firkin: { available: "yes", scheme: "firkin", uris: [result.uri] },
    });
    expect(result.resolved.url).toBe(
      `https://firkin.example/.well-known/rasl/${result.cid}`,
    );
    expect(result.authedParsed?.token).toBe("ucan-token");
    expect(result.authedResolved.url).toBe(
      `https://firkin.example/v1/blob/${result.cid}?token=ucan-token`,
    );
    expect(result.listed).toEqual([result.uri]);
    expect(result.sources).toEqual([
      { label: "Firkin (firkin.example)", uri: "firkin://firkin.example" },
    ]);
  });

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
      // Bypass any service-worker-cached worker bundle from an earlier test
      // run; the query is ignored by the worker itself.
      input.workerQuery = () => ({ v: String(Date.now()) });
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
