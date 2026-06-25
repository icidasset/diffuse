import { describe, it, beforeAll, afterAll } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { mockServer } from "@tests/common/server.ts";
import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/https-json/worker.js";
import { buildURI, parseURI } from "~/components/input/https-json/common.js";

// Mock JSON directory listing server.
// The https-json input fetches directories with `Accept: application/json`
// and expects an array of `{ name, type: "directory" | "file" }` entries.
const FILESYSTEM = {
  "/": [
    { name: "music", type: "directory" },
    { name: "readme.txt", type: "file" },
  ],
  "/music": [
    { name: "album1", type: "directory" },
    { name: "track1.mp3", type: "file" },
  ],
  "/music/album1": [
    { name: "song1.flac", type: "file" },
    { name: "song2.mp3", type: "file" },
    { name: "cover.jpg", type: "file" },
  ],
};

let server: Deno.HttpServer;
let port: number;

beforeAll(async () => {
  const started = await mockServer((req, url) => {
    // Normalise path: ensure leading slash, no trailing slash (except root)
    let dir = url.pathname;
    if (!dir.startsWith("/")) dir = "/" + dir;
    if (dir.length > 1 && dir.endsWith("/")) dir = dir.slice(0, -1);

    const entries = (FILESYSTEM as Record<string, { name: string; type: string }[]>)[dir];
    if (!entries) return new Response("", { status: 404 });

    return new Response(JSON.stringify(entries), {
      status: 200,
      headers: { "content-type": "application/json" },
    });
  });
  server = started.server;
  port = started.port;
});

afterAll(async () => {
  await server.shutdown();
});

describe("components/input/https-json (integration)", () => {
  it("consult returns true when the server responds ok", async () => {
    const uri = buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "");
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(true);
    }
  });

  it("consult returns false when the server is unreachable", async () => {
    const uri = buildURI({ host: `127.0.0.1:${port + 9999}`, dir: "/" }, "");
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("list recurses directories and returns audio files", async () => {
    const server = { host: `127.0.0.1:${port}`, dir: "/", exclude: [] };
    const uri = buildURI(server, "");
    const parsed = parseURI(uri);
    if (!parsed) throw new Error("parseURI returned undefined");

    const tracks = await Worker.list([{
      $type: "sh.diffuse.output.track",
      id: "placeholder-1",
      kind: "placeholder",
      uri,
    }]);

    // Should find: /music/track1.mp3, /music/album1/song1.flac, /music/album1/song2.mp3
    // cover.jpg and readme.txt should be filtered out
    expect(tracks.length).toBe(3);
    const uris = tracks.map((t) => t.uri);
    expect(uris.some((u) => u.includes("track1.mp3"))).toBe(true);
    expect(uris.some((u) => u.includes("song1.flac"))).toBe(true);
    expect(uris.some((u) => u.includes("song2.mp3"))).toBe(true);
    expect(uris.some((u) => u.includes("cover.jpg"))).toBe(false);
    expect(uris.some((u) => u.includes("readme.txt"))).toBe(false);
  });

  it("list respects exclude list", async () => {
    const serverObj = { host: `127.0.0.1:${port}`, dir: "/", exclude: ["music"] };
    const uri = buildURI(serverObj, "");
    const tracks = await Worker.list([{
      $type: "sh.diffuse.output.track",
      id: "p1",
      kind: "placeholder",
      uri,
    }]);

    // "music" directory is excluded, so no audio files should be found.
    // The worker returns a placeholder track when no files are found.
    expect(tracks.length).toBe(1);
    expect(tracks[0].kind).toBe("placeholder");
  });

  it("resolve returns the HTTP URL for a track path", async () => {
    const uri = buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "/music/track1.mp3");
    const result = await Worker.resolve({ uri });
    expect(result).not.toBe(undefined);
    if (result && "url" in result) {
      expect(result.url).toContain("http://127.0.0.1");
      expect(result.url).toContain("/music/track1.mp3");
      expect(result.expiresAt).toBeGreaterThan(Date.now() / 1000);
    }
  });

  it("resolve returns undefined for a URI without path", async () => {
    const uri = buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "");
    const result = await Worker.resolve({ uri });
    expect(result).toBe(undefined);
  });

  it("groupConsult reports available for a reachable server", async () => {
    const uri = buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "/music/track1.mp3");
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    expect(result[keys[0]].available).toBe(true);
  });

  it("groupConsult reports unavailable for an unreachable server", async () => {
    const uri = buildURI({ host: `127.0.0.1:${port + 9999}`, dir: "/" }, "/track.mp3");
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    expect(result[keys[0]].available).toBe(false);
  });

  it("detach with scheme removes all https-json tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "/a.mp3") },
      { $type: "sh.diffuse.output.track", id: "2", uri: buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "/b.mp3") },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "https-json", tracks });
    expect(remaining.length).toBe(0);
  });

  it("detach with a specific server URI removes only that server's tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "/a.mp3") },
      { $type: "sh.diffuse.output.track", id: "2", uri: buildURI({ host: "other.example.com", dir: "/" }, "/b.mp3") },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: buildURI({ host: `127.0.0.1:${port}`, dir: "/" }, "/a.mp3"),
      tracks,
    });
    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });
});
