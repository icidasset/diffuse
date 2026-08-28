import { describe, it, beforeAll, afterAll } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { mockServer } from "@tests/common/server.ts";
import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/opensubsonic/worker.js";

const SONGS = [
  {
    id: "s1",
    title: "Echoes",
    artist: "Pink Floyd",
    displayArtist: "Pink Floyd",
    type: "music",
    path: "folder/echoes.mp3",
    duration: 1414,
    bitRate: 320,
    track: 1,
    discNumber: 1,
    year: 1971,
    isVideo: false,
    genres: ["Prog Rock"],
    album: "Meddle",
  },
  {
    id: "s2",
    title: "Time",
    artist: "Pink Floyd",
    displayArtist: "Pink Floyd",
    type: "music",
    path: "folder/time.mp3",
    duration: 421,
    bitRate: 320,
    track: 4,
    discNumber: 1,
    year: 1973,
    isVideo: false,
    genres: ["Prog Rock"],
    album: "The Dark Side of the Moon",
  },
];

let server: Deno.HttpServer;
let port: number;

beforeAll(async () => {
  const started = await mockServer((req, url) => {
    const p = url.pathname;

    if (p === "/rest/ping.view") {
      return new Response(
        JSON.stringify({ "subsonic-response": { status: "ok", version: "1.16.1" } }),
        { headers: { "content-type": "application/json" } },
      );
    }

    if (p === "/rest/search3.view") {
      return new Response(
        JSON.stringify({
          "subsonic-response": { status: "ok", "searchResult3": { song: SONGS } },
        }),
        { headers: { "content-type": "application/json" } },
      );
    }

    if (p === "/rest/stream.view") {
      return new Response(new Uint8Array(64), {
        status: 200,
        headers: { "content-type": "audio/mpeg" },
      });
    }

    if (p === "/rest/getCoverArt.view") {
      const png = new Uint8Array([
        0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
      ]);
      return new Response(png, {
        status: 200,
        headers: { "content-type": "image/png" },
      });
    }

    return new Response("", { status: 404 });
  });
  server = started.server;
  port = started.port;
});

afterAll(async () => {
  await server.shutdown();
});

describe("components/input/opensubsonic (integration)", () => {
  it("consult returns true when the server pings ok", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port}?tls=f`;
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(true);
    }
  });

  it("consult returns false when the server is unreachable", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port + 9999}?tls=f`;
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("list returns tracks from search3", async () => {
    // Pass a cached track for s1 with existing metadata to verify it's preserved.
    const cachedTrack: Track = {
      $type: "sh.diffuse.output.track",
      id: "cached-1",
      uri: `opensubsonic://user:pass@127.0.0.1:${port}/folder/echoes.mp3?tls=f&songId=s1`,
      tags: { title: "Echoes (Cached)", album: "Meddle" },
    };
    const tracks = await Worker.list([cachedTrack]);
    expect(tracks.length).toBe(2);

    // Cached track preserves its id and original tags
    const cached = tracks.find((t) => t.id === "cached-1");
    expect(cached).toBeDefined();
    expect(cached?.tags?.title).toBe("Echoes (Cached)");

    // New track from server has proper metadata
    const time = tracks.find((t) => t.tags?.title === "Time");
    expect(time).toBeDefined();
    expect(time?.tags?.artist).toBe("Pink Floyd");
    expect(time?.tags?.album).toBe("The Dark Side of the Moon");
    expect(time?.kind).toBe("music");
    expect(time?.stats?.duration).toBe(421_000);
  });

  it("resolve returns the stream URL for a songId", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port}?tls=f&songId=s1`;
    const result = await Worker.resolve({ uri });
    expect(result).not.toBe(undefined);
    if (result && "url" in result) {
      expect(result.url).toContain("/rest/stream.view");
      expect(result.url).toContain("id=s1");
      expect(result.expiresAt).toBe(Infinity);
    }
  });

  it("resolve returns undefined for a URI without songId", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port}?tls=f`;
    const result = await Worker.resolve({ uri });
    expect(result).toBe(undefined);
  });

  it("artwork returns image bytes for a songId", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port}?tls=f&songId=s1`;
    const result = await Worker.artwork(uri);
    expect(result).not.toBe(null);
    if (result) {
      expect(result.length).toBeGreaterThan(0);
      expect(Array.from(result.slice(0, 4))).toEqual([0x89, 0x50, 0x4E, 0x47]);
    }
  });

  it("artwork returns null for a URI without songId", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port}?tls=f`;
    const result = await Worker.artwork(uri);
    expect(result).toBe(null);
  });

  it("groupConsult reports available for a reachable server", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port}?tls=f&songId=s1`;
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    const grouping = result[keys[0]];
    expect(grouping.available).toBe(true);
    expect(grouping.uris).toEqual([uri]);
  });

  it("groupConsult reports unavailable for an unreachable server", async () => {
    const uri = `opensubsonic://user:pass@127.0.0.1:${port + 9999}?tls=f&songId=s1`;
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    const grouping = result[keys[0]];
    expect(grouping.available).toBe(false);
  });

  it("detach with scheme removes all opensubsonic tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: `opensubsonic://user:pass@127.0.0.1:${port}?tls=f&songId=s1` },
      { $type: "sh.diffuse.output.track", id: "2", uri: `opensubsonic://user:pass@127.0.0.1:${port}?tls=f&songId=s2` },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "opensubsonic", tracks });
    expect(remaining.length).toBe(0);
  });

  it("detach with a specific server URI removes only that server's tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: `opensubsonic://user:pass@127.0.0.1:${port}?tls=f&songId=s1` },
      { $type: "sh.diffuse.output.track", id: "2", uri: "opensubsonic://user:pass@other.example.com?tls=t&songId=s3" },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: `opensubsonic://user:pass@127.0.0.1:${port}?tls=f`,
      tracks,
    });
    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });
});
