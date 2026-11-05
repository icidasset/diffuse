// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/wnfs/files (broadcast client)", () => {
  it("calls the output element's file actions over the broadcast channel", async () => {
    const result = await testWeb(async () => {
      const { outputChannelName, StorageClient } = await import(
        "~/components/output/bytes/wnfs/files.js"
      );
      const { rpc } = await import("~/common/worker.js");

      const options = { group: "files-test", namespace: "files-test" };
      const server = new BroadcastChannel(outputChannelName(options));

      const files = new Map([["a.mp3", new Uint8Array([1, 2, 3])]]);

      const channel = rpc(server, {
        storageStatus: () => ({ connected: true, did: "did:key:test" }),
        storageList: () =>
          [...files.keys()].map((name) => ({ name, modified: 0 })),
        storageRead: (name) => files.get(name),
        storageWrite: (name, bytes) => {
          files.set(name, bytes);
          return "root";
        },
        storageDelete: (name) => {
          files.delete(name);
          return null;
        },
      });

      const client = new StorageClient(options);

      const status = await client.status();
      const list = await client.list();
      const bytes = await client.read("a.mp3");
      const root = await client.write("b.mp3", new Uint8Array([4, 5]));
      await client.remove("a.mp3");

      client.close();
      channel.destroy();
      server.close();

      return {
        status,
        list: list.map((item) => item.name),
        bytes: [...bytes],
        root,
        remaining: [...files.keys()],
      };
    });

    expect(result).toEqual({
      status: { connected: true, did: "did:key:test" },
      list: ["a.mp3"],
      bytes: [1, 2, 3],
      root: "root",
      remaining: ["b.mp3"],
    });
  });
});
