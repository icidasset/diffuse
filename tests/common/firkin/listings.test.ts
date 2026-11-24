// @ts-nocheck -- the fake channel and lister are intentionally minimal

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { createListingStore, listingKey } from "~/common/firkin/listings.js";

/** A pair of BroadcastChannel stand-ins that relay to each other. */
class FakeChannel extends EventTarget {
  peer;

  /** @param {unknown} data */
  postMessage(data) {
    this.peer?.dispatchEvent(new MessageEvent("message", { data }));
  }

  close() {}
}

function pair() {
  const a = new FakeChannel();
  const b = new FakeChannel();

  a.peer = b;
  b.peer = a;

  return [a, b];
}

/** @param {Record<string, string>} names */
function lister(names) {
  const calls = { count: 0 };

  return {
    calls,
    rb: {
      registryNames: async () => {
        calls.count++;
        return names;
      },
    },
  };
}

describe("common/firkin/listings", () => {
  it("scopes keys by server, account and namespace", () => {
    const key = listingKey(
      { apiBase: "https://a", did: "did:key:z1" },
      "namespace",
    );

    expect(key).not.toBe(
      listingKey({ apiBase: "https://b", did: "did:key:z1" }, "namespace"),
    );
    expect(key).not.toBe(
      listingKey({ apiBase: "https://a", did: "did:key:z2" }, "namespace"),
    );
    expect(key).not.toBe(
      listingKey({ apiBase: "https://a", did: "did:key:z1" }, "other"),
    );
  });

  it("caches a listing and coalesces concurrent reads", async () => {
    const store = createListingStore({ channel: new FakeChannel() });
    const { rb, calls } = lister({ "tracks.device": "bafy" });

    const [a, b] = await Promise.all([
      store.names(rb, "key", "namespace"),
      store.names(rb, "key", "namespace"),
    ]);

    expect(calls.count).toBe(1);
    expect(a).toEqual({ "tracks.device": "bafy" });
    expect(b).toEqual(a);

    expect(await store.names(rb, "key", "namespace")).toEqual(a);
    expect(calls.count).toBe(1);

    store.close();
  });

  it("refetches after the ttl", async () => {
    const store = createListingStore({ channel: new FakeChannel() });
    const { rb, calls } = lister({ "tracks.device": "bafy" });

    await store.names(rb, "key", "namespace", 0);
    await store.names(rb, "key", "namespace", 0);

    expect(calls.count).toBe(2);

    store.close();
  });

  it("adopts a listing fetched by another store", async () => {
    const [a, b] = pair();
    const storeA = createListingStore({ channel: a });
    const storeB = createListingStore({ channel: b });
    const left = lister({ "tracks.device": "left" });
    const right = lister({ "tracks.device": "right" });

    const fromA = await storeA.names(left.rb, "key", "namespace");
    const fromB = await storeB.names(right.rb, "key", "namespace");

    expect(right.calls.count).toBe(0);
    expect(fromB).toEqual(fromA);

    storeA.close();
    storeB.close();
  });

  it("does not adopt another key's listing", async () => {
    const [a, b] = pair();
    const storeA = createListingStore({ channel: a });
    const storeB = createListingStore({ channel: b });
    const left = lister({ "tracks.device": "left" });
    const right = lister({ "tracks.device": "right" });

    await storeA.names(left.rb, "key-a", "namespace");
    const fromB = await storeB.names(right.rb, "key-b", "namespace");

    expect(right.calls.count).toBe(1);
    expect(fromB).toEqual({ "tracks.device": "right" });

    storeA.close();
    storeB.close();
  });

  it("invalidates across stores", async () => {
    const [a, b] = pair();
    const storeA = createListingStore({ channel: a });
    const storeB = createListingStore({ channel: b });
    const left = lister({ "tracks.device": "left" });
    const right = lister({ "tracks.device": "right" });

    await storeA.names(left.rb, "key", "namespace");
    storeA.invalidate("key");
    await storeB.names(right.rb, "key", "namespace");

    expect(right.calls.count).toBe(1);

    storeA.close();
    storeB.close();
  });
});
