import { describe, it, beforeAll, afterAll } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { mockServer } from "@tests/common/server.ts";
import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/icecast/worker.js";

const METAINT = 16;
const META_STRING = "StreamTitle='Pink Floyd - Time';StreamUrl='';";
const META_PADDED = (() => {
  const encoded = new TextEncoder().encode(META_STRING);
  const len = Math.ceil(encoded.length / 16) * 16;
  const out = new Uint8Array(len);
  out.set(encoded);
  return out;
})();
const META_LENGTH_BYTE = new Uint8Array([META_PADDED.length / 16]);

function streamBody() {
  const audio = new Uint8Array(METAINT).fill(0xAA);
  return new Blob([audio, META_LENGTH_BYTE, META_PADDED]).stream();
}

// Two servers on distinct ports so the per-host consult cache doesn't bleed
// between scenarios.
let goodServer: Deno.HttpServer;
let goodPort: number;
let plainServer: Deno.HttpServer;
let plainPort: number;
let deadPort: number;

beforeAll(async () => {
  const good = await mockServer((_req, url) => {
    if (url.pathname === "/stream.mp3") {
      return new Response(streamBody(), {
        status: 200,
        headers: {
          "Content-Type": "audio/mpeg",
          "icy-name": "TestRadio",
          "icy-genre": "Prog Rock",
          "icy-br": "128",
          "icy-metaint": String(METAINT),
        },
      });
    }
    return new Response("", { status: 404 });
  });
  goodServer = good.server;
  goodPort = good.port;

  const plain = await mockServer((_req, url) => {
    if (url.pathname === "/noicy") {
      return new Response(new Uint8Array(32), {
        status: 200,
        headers: { "Content-Type": "audio/mpeg" },
      });
    }
    return new Response("", { status: 404 });
  });
  plainServer = plain.server;
  plainPort = plain.port;

  // A port that nothing listens on.
  deadPort = plainPort + 1;
});

afterAll(async () => {
  await goodServer.shutdown();
  await plainServer.shutdown();
});

describe("components/input/icecast (integration)", () => {
  it("consult returns true for a live stream with ICY metadata", async () => {
    const result = await Worker.consult(`icecast://127.0.0.1:${goodPort}/stream.mp3?tls=0`);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(true);
    }
  });

  it("consult returns false for a stream without icy-metaint", async () => {
    const result = await Worker.consult(`icecast://127.0.0.1:${plainPort}/noicy?tls=0`);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("consult returns false for an unreachable host", async () => {
    const result = await Worker.consult(`icecast://127.0.0.1:${deadPort}/stream.mp3?tls=0`);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("resolve returns the HTTPS stream URL by default", async () => {
    const resolved = await Worker.resolve({ uri: "icecast://radio.example.com/stream.mp3" });
    expect(resolved).not.toBe(undefined);
    if (resolved && "url" in resolved) {
      expect(resolved.url).toContain("https://");
      expect(resolved.url).toContain("radio.example.com");
      expect(resolved.expiresAt).toBeGreaterThan(Date.now() / 1000);
    }
  });

  it("resolve returns an HTTP stream URL when tls=0", async () => {
    const resolved = await Worker.resolve({
      uri: `icecast://127.0.0.1:${goodPort}/stream.mp3?tls=0`,
    });
    expect(resolved).not.toBe(undefined);
    if (resolved && "url" in resolved) {
      expect(resolved.url).toContain("http://127.0.0.1");
      expect(resolved.url).toContain("/stream.mp3");
    }
  });

  it("list enriches tracks with ICY metadata from the stream", async () => {
    const tracks: Track[] = [{
      $type: "sh.diffuse.output.track",
      id: "t1",
      uri: `icecast://127.0.0.1:${goodPort}/stream.mp3?tls=0`,
    }];
    const refreshed = await Worker.list(tracks);
    expect(refreshed.length).toBe(1);
    expect(refreshed[0].kind).toBe("stream");
    expect(refreshed[0].tags?.title).toBe("TestRadio");
    expect(refreshed[0].tags?.genres).toEqual(["Prog Rock"]);
    expect(refreshed[0].stats?.bitrate).toBe(128_000);
  });

  it("groupConsult reports available for a reachable host", async () => {
    const result = await Worker.groupConsult([
      `icecast://127.0.0.1:${goodPort}/stream.mp3?tls=0`,
    ]);
    const key = `icecast://127.0.0.1:${goodPort}`;
    expect(result[key]?.available).toBe(true);
    expect(result[key]?.uris).toEqual([
      `icecast://127.0.0.1:${goodPort}/stream.mp3?tls=0`,
    ]);
  });

  it("groupConsult reports unavailable for an unreachable host", async () => {
    const result = await Worker.groupConsult([
      `icecast://127.0.0.1:${deadPort}/stream.mp3?tls=0`,
    ]);
    const key = `icecast://127.0.0.1:${deadPort}`;
    expect(result[key]?.available).toBe(false);
    if (!result[key]?.available) {
      expect(result[key]?.reason).toBeDefined();
    }
  });

  it("detach with scheme removes all icecast tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: `icecast://127.0.0.1:${goodPort}/a?tls=0` },
      { $type: "sh.diffuse.output.track", id: "2", uri: `icecast://127.0.0.1:${goodPort}/b?tls=0` },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "icecast", tracks });
    expect(remaining.length).toBe(0);
  });

  it("detach with a specific host URI removes only that host's tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: `icecast://127.0.0.1:${goodPort}/a?tls=0` },
      { $type: "sh.diffuse.output.track", id: "2", uri: "icecast://other.example.com/c" },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: `icecast://127.0.0.1:${goodPort}/a?tls=0`,
      tracks,
    });
    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });
});
