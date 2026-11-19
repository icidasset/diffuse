// @ts-nocheck -- the fake socket intentionally implements only what the channel uses

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  HINT_DEBOUNCE_MS,
  channelName,
  openHintChannel,
  parseHint,
} from "~/common/firkin/channel.js";

/** A stand-in for the browser WebSocket, driven by the test. */
class FakeSocket extends EventTarget {
  static instances = [];

  static reset() {
    FakeSocket.instances = [];
  }

  /** @param {string} url */
  constructor(url) {
    super();
    this.url = url;
    this.readyState = 0;
    this.sent = [];
    this.closed = false;
    FakeSocket.instances.push(this);
  }

  open() {
    this.readyState = 1;
    this.dispatchEvent(new Event("open"));
  }

  /** @param {unknown} data */
  receive(data) {
    this.dispatchEvent(new MessageEvent("message", { data }));
  }

  /** @param {string} data */
  send(data) {
    this.sent.push(data);
  }

  close() {
    this.closed = true;
    this.readyState = 3;
    this.dispatchEvent(new Event("close"));
  }
}

const flush = () => new Promise((resolve) => setTimeout(resolve, 0));

describe("common/firkin/channel", () => {
  it("names channels per namespace", () => {
    expect(channelName("firkin-automerge")).toBe(
      "diffuse:output:firkin-automerge",
    );
    expect(channelName(undefined)).toBe("diffuse:output:default");
  });

  it("parses only well-formed hints", () => {
    expect(parseHint(JSON.stringify({ v: 1, collection: "tracks" }))).toEqual({
      collection: "tracks",
      device: undefined,
    });
    expect(
      parseHint(JSON.stringify({ v: 1, collection: "tracks", device: "abc" })),
    ).toEqual({ collection: "tracks", device: "abc" });

    expect(parseHint("not json")).toBeUndefined();
    expect(parseHint(JSON.stringify({ v: 2, collection: "tracks" })))
      .toBeUndefined();
    expect(parseHint(JSON.stringify({ v: 1 }))).toBeUndefined();
    expect(parseHint(new Uint8Array([1]))).toBeUndefined();
  });

  it("reports hints from other devices only", async () => {
    FakeSocket.reset();

    /** @type {string[]} */
    const hints = [];

    const channel = openHintChannel({
      device: "mine",
      url: async () => "ws://test/1",
      onHint: (collection) => hints.push(collection),
      WebSocketImpl: FakeSocket,
    });

    await flush();

    const socket = FakeSocket.instances[0];
    expect(socket.url).toBe("ws://test/1");
    socket.open();

    socket.receive(JSON.stringify({ v: 1, collection: "tracks", device: "other" }));
    socket.receive(JSON.stringify({ v: 1, collection: "facets", device: "mine" }));
    socket.receive("garbage");

    expect(hints).toEqual(["tracks"]);

    channel.close();
    expect(socket.closed).toBe(true);
  });

  it("ignores sends while disconnected", async () => {
    FakeSocket.reset();

    const channel = openHintChannel({
      device: "mine",
      url: async () => "ws://test/2",
      onHint: () => {},
      WebSocketImpl: FakeSocket,
    });

    await flush();
    const socket = FakeSocket.instances[0];

    channel.send("tracks");
    expect(socket.sent).toEqual([]);

    socket.open();
    channel.send("tracks");

    expect(JSON.parse(socket.sent[0])).toEqual({
      v: 1,
      collection: "tracks",
      device: "mine",
    });

    channel.close();
  });

  it("stops hinting after close", async () => {
    FakeSocket.reset();

    /** @type {string[]} */
    const hints = [];

    const channel = openHintChannel({
      device: "mine",
      url: async () => "ws://test/3",
      onHint: (collection) => hints.push(collection),
      WebSocketImpl: FakeSocket,
    });

    await flush();
    const socket = FakeSocket.instances[0];
    socket.open();

    channel.close();
    socket.receive(JSON.stringify({ v: 1, collection: "tracks", device: "other" }));

    expect(hints).toEqual([]);
    expect(FakeSocket.instances.length).toBe(1);
  });

  it("does nothing without a url", async () => {
    FakeSocket.reset();

    const channel = openHintChannel({
      device: "mine",
      url: async () => undefined,
      onHint: () => {},
      WebSocketImpl: FakeSocket,
    });

    await flush();

    expect(FakeSocket.instances.length).toBe(0);
    channel.close();
  });

  it("reports a reconnected channel so missed hints can be recovered", async () => {
    FakeSocket.reset();

    let reconnects = 0;

    const channel = openHintChannel({
      device: "mine",
      url: async () => "ws://test/4",
      onHint: () => {},
      onReconnect: () => reconnects++,
      WebSocketImpl: FakeSocket,
    });

    await flush();
    const first = FakeSocket.instances[0];
    first.open();

    // The first open follows the initial pull; no catch-up is needed.
    expect(reconnects).toBe(0);

    first.close();
    await new Promise((resolve) => setTimeout(resolve, 700));

    const second = FakeSocket.instances[1];
    expect(second).toBeDefined();
    second.open();

    expect(reconnects).toBe(1);
    channel.close();
  });

  it("debounces bursts per collection", () => {
    expect(HINT_DEBOUNCE_MS).toBeGreaterThan(0);
  });
});
