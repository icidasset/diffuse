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

      const files = new Map([
        [
          "a.mp3",
          {
            bytes: new Uint8Array([1, 2, 3]),
            cid: "bafk-a",
            mime: "audio/mpeg",
            modified: 0,
            size: 3,
          },
        ],
      ]);

      const channel = rpc(server, {
        storageStatus: () => ({ connected: true, did: "did:key:test" }),
        storageListFiles: () =>
          [...files.entries()].map(([name, file]) => ({
            name,
            cid: file.cid,
            mime: file.mime,
            modified: file.modified,
            size: file.size,
          })),
        storageRead: (name) => files.get(name)?.bytes,
        storageFileURL: (name, expireIn) =>
          `https://firkin.test/blob/${
            files.get(name)?.cid
          }?token=x&expire=${expireIn}`,
        storageUploadFile: (name, bytes, mime) => {
          const cid = `bafk-${name}`;
          files.set(name, {
            bytes,
            cid,
            mime,
            modified: 0,
            size: bytes.byteLength,
          });
          return { cid };
        },
        storageDeleteFile: (name) => {
          files.delete(name);
        },
      });

      const client = new StorageClient(options);

      const status = await client.status();
      const list = await client.listFiles();
      const bytes = await client.read("a.mp3");
      const url = await client.fileURL("a.mp3", 123);
      const uploaded = await client.uploadFile(
        "b.mp3",
        new Uint8Array([4, 5]),
        "audio/mpeg",
      );
      await client.deleteFile("a.mp3");

      client.close();
      channel.destroy();
      server.close();

      return {
        status,
        list: list.map((item) => item.name),
        bytes: [...bytes],
        url,
        uploaded,
        remaining: [...files.keys()],
      };
    });

    expect(result).toEqual({
      status: { connected: true, did: "did:key:test" },
      list: ["a.mp3"],
      bytes: [1, 2, 3],
      url: "https://firkin.test/blob/bafk-a?token=x&expire=123",
      uploaded: { cid: "bafk-b.mp3" },
      remaining: ["b.mp3"],
    });
  });
});
