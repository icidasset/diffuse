// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/input/wnfs-firkin", () => {
  it("lists, resolves and consults through the output broadcast channel", async () => {
    const result = await testWeb(async () => {
      const group = "di-wnfs-firkin-test";
      const namespace = "test";

      const { outputChannelName, SOURCE_URI } = await import(
        "~/components/output/bytes/wnfs/files.js"
      );
      const { rpc } = await import("~/common/worker.js");
      const mod = await import(
        "~/components/input/wnfs-firkin/element.js"
      );

      const server = new BroadcastChannel(
        outputChannelName({ group, namespace }),
      );
      const files = new Map([["a.mp3", new Uint8Array([1, 2, 3])]]);

      const channel = rpc(server, {
        storageStatus: () => ({ connected: true, did: "did:key:test" }),
        storageListFiles: () =>
          [...files.keys()].map((name) => ({
            name,
            cid: "bafkreitest",
            size: 3,
            mime: "audio/mpeg",
            modified: 0,
          })),
        storageRead: (name) => files.get(name),
        storageFileURL: (name) =>
          `https://firkin.test/blob/bafkreitest?token=x&name=${name}`,
        storageUploadFile: () => ({ cid: "bafkreitest" }),
        storageDeleteFile: () => undefined,
      });

      const input = new mod.CLASS();
      input.setAttribute("group", group);
      input.setAttribute("namespace", namespace);
      document.body.append(input);

      const consult = await input.consult(SOURCE_URI);
      const tracks = await input.list([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: SOURCE_URI,
          kind: "placeholder",
        },
      ]);
      const resolved = await input.resolve({ uri: tracks[0].uri });
      const artwork = await input.artwork(tracks[0].uri);
      const sources = input.sources(tracks);

      channel.destroy();
      server.close();

      return {
        consult,
        tracks: tracks.map((track) => ({
          hasId: typeof track.id === "string" && track.id.length > 0,
          uri: track.uri,
        })),
        resolvedUrl: resolved && "url" in resolved ? resolved.url : null,
        artwork,
        sources,
      };
    });

    expect(result.consult).toEqual({ supported: true, consult: "yes" });
    expect(result.tracks).toEqual([
      { hasId: true, uri: "wnfs+firkin://me/files/a.mp3" },
    ]);
    expect(result.resolvedUrl).toBe(
      "https://firkin.test/blob/bafkreitest?token=x&name=a.mp3",
    );
    expect(result.artwork).toBe(null);
    expect(result.sources).toEqual([
      { label: "Firkin + WNFS", uri: "wnfs+firkin://me" },
    ]);
  });

  it("keeps a placeholder when the storage has no audio files", async () => {
    const result = await testWeb(async () => {
      const group = "di-wnfs-firkin-test";
      const namespace = "test";

      const { outputChannelName, SOURCE_URI } = await import(
        "~/components/output/bytes/wnfs/files.js"
      );
      const { rpc } = await import("~/common/worker.js");
      const mod = await import(
        "~/components/input/wnfs-firkin/element.js"
      );

      const server = new BroadcastChannel(
        outputChannelName({ group, namespace }),
      );

      const channel = rpc(server, {
        storageStatus: () => ({ connected: true }),
        storageListFiles: () => [],
        storageRead: () => undefined,
        storageFileURL: () => null,
        storageUploadFile: () => null,
        storageDeleteFile: () => undefined,
      });

      const input = new mod.CLASS();
      input.setAttribute("group", group);
      input.setAttribute("namespace", namespace);
      document.body.append(input);

      const tracks = await input.list([
        {
          $type: "sh.diffuse.output.track",
          id: "keep-me",
          uri: SOURCE_URI,
          kind: "placeholder",
        },
      ]);

      channel.destroy();
      server.close();

      return tracks.map((track) => track.id);
    });

    expect(result).toEqual(["keep-me"]);
  });
});
