// @ts-nocheck -- DAG helper types are intentionally loose in this test

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import * as dagcbor from "@ipld/dag-cbor";
import * as dagpb from "@ipld/dag-pb";
import { CID } from "multiformats/cid";
import { sha256 } from "multiformats/hashes/sha2";

import {
  carFile,
  Firkin,
} from "~/vendor/firkin/index.js";

/** @param {Uint8Array} bytes @param {number} codec */
async function block(bytes: Uint8Array, codec: number) {
  const cid = CID.createV1(codec, await sha256.digest(bytes));
  return { cid, bytes };
}

describe("Firkin SDK DAG sync", () => {
  it("dagPull walks dag-pb and dag-cbor links and authenticates", async () => {
    const leaf = await block(new TextEncoder().encode("leaf"), 0x55);
    const childBytes = dagcbor.encode({ child: leaf.cid });
    const child = await block(childBytes, 0x71);
    const rootNode = dagpb.createNode(new Uint8Array([8, 1]), [
      { Name: "child", Hash: child.cid },
      { Name: "leaf", Hash: leaf.cid },
    ]);
    const root = await block(dagpb.encode(rootNode), 0x70);

    let auth: string | undefined;
    const fetchImpl = async (_url: string, init?: RequestInit) => {
      auth = (init?.headers as Record<string, string> | undefined)
        ?.Authorization;
      const car = carFile(root.cid, [
        [root.cid, root.bytes],
        [child.cid, child.bytes],
        [leaf.cid, leaf.bytes],
      ]);
      return new Response(car, {
        status: 200,
        headers: { "Content-Type": "application/vnd.ipld.car" },
      });
    };

    const rb = new Firkin(
      "http://example",
      async () => new Uint8Array([1]),
      fetchImpl,
    );

    const blocks = await rb.dagPull(root.cid.toString());

    expect(auth?.startsWith("Bearer ")).toBe(true);
    expect([...blocks.keys()].sort()).toEqual(
      [root.cid.toString(), child.cid.toString(), leaf.cid.toString()].sort(),
    );
  });

  it("dagPush runs the server's multi-round Bloom loop", async () => {
    const leaf = await block(new TextEncoder().encode("leaf"), 0x55);
    const childBytes = dagcbor.encode({ child: leaf.cid });
    const child = await block(childBytes, 0x71);
    const rootNode = dagpb.createNode(new Uint8Array([8, 1]), [
      { Name: "child", Hash: child.cid },
      { Name: "leaf", Hash: leaf.cid },
    ]);
    const root = await block(dagpb.encode(rootNode), 0x70);

    let pushes = 0;
    const fetchImpl = async (url: string, init?: RequestInit) => {
      if (!url.includes("/dag/push/")) {
        throw new Error(`unexpected request: ${url}`);
      }
      const headers = init?.headers as Record<string, string> | undefined;
      if (!headers?.Authorization?.startsWith("Bearer ")) {
        throw new Error("push was not authenticated");
      }
      pushes++;

      const body = pushes === 1
        ? dagcbor.encode({
          sr: [child.cid],
          bk: 1,
          bb: new Uint8Array([0]),
        })
        : dagcbor.encode({ sr: [], bk: 0, bb: new Uint8Array() });

      return new Response(body, { status: pushes === 1 ? 202 : 200 });
    };

    const rb = new Firkin(
      "http://example",
      async () => new Uint8Array([1]),
      fetchImpl,
    );

    await rb.dagPush(root.cid.toString(), [
      [root.cid.toString(), root.bytes],
      [child.cid.toString(), child.bytes],
      [leaf.cid.toString(), leaf.bytes],
    ]);

    expect(pushes).toBe(2);
  });
});
