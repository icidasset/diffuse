// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/upload/wnfs-firkin", () => {
  it("uploads and deletes through the output broadcast channel", async () => {
    const result = await testWeb(async () => {
      const group = "du-wnfs-firkin-test";
      const namespace = "test";

      const { outputChannelName, SOURCE_URI } = await import(
        "~/components/output/bytes/wnfs/files.js"
      );
      const { rpc } = await import("~/common/worker.js");
      const mod = await import(
        "~/components/upload/wnfs-firkin/element.js"
      );

      const server = new BroadcastChannel(
        outputChannelName({ group, namespace }),
      );
      const writes = [];
      const deletes = [];

      const channel = rpc(server, {
        storageStatus: () => ({ connected: true }),
        storageListFiles: () => [],
        storageRead: () => undefined,
        storageFileURL: () => null,
        storageUploadFile: (name, bytes, mime) => {
          writes.push({ name, bytes: [...bytes], mime });
          return { cid: "bafkreiuploaded" };
        },
        storageDeleteFile: (name) => {
          deletes.push(name);
        },
      });

      const upload = new mod.CLASS();
      upload.setAttribute("group", group);
      upload.setAttribute("namespace", namespace);
      document.body.append(upload);

      const consult = await upload.consult(SOURCE_URI);
      const uri = await upload.upload({
        uri: SOURCE_URI,
        file: new File([new Uint8Array([7, 8])], "bafktest.mp3"),
      });
      await upload.delete(uri);

      channel.destroy();
      server.close();

      return { consult, uri, writes, deletes };
    });

    expect(result.consult).toEqual({ supported: true, consult: "yes" });
    expect(result.uri).toBe("wnfs+firkin://me/files/bafktest.mp3");
    expect(result.writes).toEqual([
      { name: "bafktest.mp3", bytes: [7, 8], mime: "audio/mpeg" },
    ]);
    expect(result.deletes).toEqual(["bafktest.mp3"]);
  });
});
