// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/wnfs/blockstore", () => {
  it("prefetches the DAG with /dag/pull before falling back to /blob", async () => {
    const result = await testWeb(async () => {
      const { FirkinBlockstore } = await import(
        "~/components/output/bytes/wnfs/blockstore.js"
      );

      const cid = "bafkreitestblock";
      const blocks = new Map([[cid, new Uint8Array([1, 2, 3])]]);

      let pulledRoot;
      let individualReads = 0;

      const rb = {
        async dagPull(root) {
          pulledRoot = root;
          return blocks;
        },
        async get() {
          individualReads++;
          return new Uint8Array();
        },
        async upload() {
          throw new Error("upload not expected");
        },
      };

      const store = new FirkinBlockstore(rb, "bafkreitestroot");
      const bytes = await store.get({ code: 0x55, toString: () => cid });

      return {
        pulledRoot,
        individualReads,
        bytes: [...bytes],
      };
    });

    expect(result.pulledRoot).toBe("bafkreitestroot");
    expect(result.individualReads).toBe(0);
    expect(result.bytes).toEqual([1, 2, 3]);
  });

  it("uploads dirty blocks with /dag/push using their real CIDs", async () => {
    const result = await testWeb(async () => {
      const { FirkinBlockstore } = await import(
        "~/components/output/bytes/wnfs/blockstore.js"
      );

      let pushed;

      const rb = {
        async dagPush(root, blocks) {
          pushed = { root, blocks };
        },
        async upload() {
          throw new Error("individual upload should not be used");
        },
        async dagPull() {
          return new Map();
        },
        async get() {
          throw new Error("read not expected");
        },
      };

      const store = new FirkinBlockstore(rb);
      // The SDK requires the CAR root to be one of the pushed blocks.
      store.put(
        { code: 0x70, toString: () => "bafkreitestroot" },
        new Uint8Array([1]),
      );
      store.put(
        { code: 0x55, toString: () => "bafkreitestblock" },
        new Uint8Array([9, 9]),
      );

      await store.flush("bafkreitestroot");

      const block = [...pushed.blocks].find(
        ([cid]) => cid === "bafkreitestblock",
      );

      return {
        root: pushed?.root,
        count: [...(pushed?.blocks ?? [])].length,
        cid: block?.[0],
        bytes: [...(block?.[1] ?? [])],
      };
    });

    expect(result.root).toBe("bafkreitestroot");
    expect(result.count).toBe(2);
    expect(result.cid).toBe("bafkreitestblock");
    expect(result.bytes).toEqual([9, 9]);
  });

  it("acts as the SDK's authenticated pull block cache", async () => {
    const result = await testWeb(async () => {
      const { FirkinBlockstore } = await import(
        "~/components/output/bytes/wnfs/blockstore.js"
      );
      const { Firkin, carFile, cidForBytes } = await import(
        "~/vendor/firkin/index.js"
      );
      const { CID } = await import("~/vendor/multiformats/cid.js");

      const rootBytes = new Uint8Array([1]);
      const childBytes = new Uint8Array([2]);
      const rootCid = cidForBytes(rootBytes, { codec: 0x55 });
      const childCid = cidForBytes(childBytes, { codec: 0x55 });

      const car = carFile(CID.parse(rootCid), [
        [CID.parse(rootCid), rootBytes],
        [CID.parse(childCid), childBytes],
      ]);

      let auth;
      const fetchImpl = async (_url, init) => {
        auth = init?.headers?.Authorization;
        return new Response(car, { status: 200 });
      };

      const store = new FirkinBlockstore(null, rootCid);
      const blockCache = {
        get: (cid) => store.peek(cid),
        set: (cid, bytes) => store.seed(cid, bytes),
        keys: () => store.keys(),
      };
      const rb = new Firkin(
        "http://example",
        async () => new Uint8Array([1]),
        fetchImpl,
        blockCache,
      );

      const blocks = await rb.dagPull(rootCid);

      return {
        auth: auth?.startsWith("Bearer ") ?? false,
        reachable: blocks.size,
        rootCached: Boolean(await store.peek(rootCid)),
        childCached: Boolean(await store.peek(childCid)),
      };
    });

    expect(result).toEqual({
      auth: true,
      reachable: 1,
      rootCached: true,
      childCached: true,
    });
  });

  it("persists blocks in IndexedDB across blockstore instances", async () => {
    const result = await testWeb(async () => {
      const { FirkinBlockstore } = await import(
        "~/components/output/bytes/wnfs/blockstore.js"
      );

      const cid = "bafkpersisted";
      const rb = {
        async get() {
          throw new Error("network read should not happen");
        },
        async upload() {
          throw new Error("upload not expected");
        },
        async dagPull() {
          return new Map();
        },
        async dagPush() {},
      };

      const first = new FirkinBlockstore(rb);
      first.put({ code: 0x55, toString: () => cid }, new Uint8Array([7, 7]));
      await first.close();

      const second = new FirkinBlockstore(rb);
      return [...(await second.get({ code: 0x55, toString: () => cid }))];
    });

    expect(result).toEqual([7, 7]);
  });
});
