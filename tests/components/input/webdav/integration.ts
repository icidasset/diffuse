import { describe, it, beforeAll, afterAll } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { mockServer } from "@tests/common/server.ts";
import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/webdav/worker.js";
import { buildURI, parseURI } from "~/components/input/webdav/common.js";

// Mock WebDAV server that responds to PROPFIND with XML.
// The webdav input sends PROPFIND with Depth:1 and parses the multistatus XML.
function propfindResponse(baseUrl: string, dir: string): string {
  const entries: Record<string, { name: string; isCollection: boolean }[]> = {
    "/": [
      { name: "music", isCollection: true },
      { name: "doc.txt", isCollection: false },
    ],
    "/music": [
      { name: "album", isCollection: true },
      { name: "track1.mp3", isCollection: false },
    ],
    "/music/album": [
      { name: "song1.flac", isCollection: false },
      { name: "song2.mp3", isCollection: false },
      { name: "cover.jpg", isCollection: false },
    ],
  };

  const normDir = dir.endsWith("/") && dir.length > 1 ? dir.slice(0, -1) : dir;
  const items = entries[normDir] || [];

  const responses = items.map((item) => {
    const href = `${normDir === "/" ? "" : normDir}/${encodeURIComponent(item.name)}${item.isCollection ? "/" : ""}`;
    const resourcetype = item.isCollection
      ? "<resourcetype><collection/></resourcetype>"
      : "<resourcetype/>";
    return `
      <response>
        <href>${href}</href>
        <propstat>
          <prop>${resourcetype}</prop>
          <status>HTTP/1.1 200 OK</status>
        </propstat>
      </response>`;
  }).join("");

  return `<?xml version="1.0" encoding="utf-8" ?>
    <multistatus xmlns="DAV:">
      ${responses}
    </multistatus>`;
}

let server: Deno.HttpServer;
let port: number;

beforeAll(async () => {
  const started = await mockServer((req, url) => {
    if (req.method === "PROPFIND") {
      const dir = url.pathname;
      return new Response(propfindResponse(`http://127.0.0.1:${port}`, dir), {
        status: 207,
        headers: { "content-type": "application/xml; charset=utf-8" },
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

describe("components/input/webdav (integration)", () => {
  it("consult returns true when the server responds with 207", async () => {
    const uri = buildURI(
      { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
      "",
    );
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(true);
    }
  });

  it("consult returns false when the server is unreachable", async () => {
    const uri = buildURI(
      { username: "user", password: "pass", host: `127.0.0.1:${port + 9999}`, dir: "/" },
      "",
    );
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("list recurses PROPFIND and returns audio files", async () => {
    const serverObj = { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" };
    const uri = buildURI(serverObj, "");
    const tracks = await Worker.list([{
      $type: "sh.diffuse.output.track",
      id: "p1",
      kind: "placeholder",
      uri,
    }]);

    // Should find: /music/track1.mp3, /music/album/song1.flac, /music/album/song2.mp3
    // doc.txt and cover.jpg should be filtered out
    expect(tracks.length).toBe(3);
    const uris = tracks.map((t) => t.uri);
    expect(uris.some((u) => u.includes("track1.mp3"))).toBe(true);
    expect(uris.some((u) => u.includes("song1.flac"))).toBe(true);
    expect(uris.some((u) => u.includes("song2.mp3"))).toBe(true);
    expect(uris.some((u) => u.includes("cover.jpg"))).toBe(false);
    expect(uris.some((u) => u.includes("doc.txt"))).toBe(false);
  });

  it("resolve returns the HTTP URL with basic-auth query param", async () => {
    const uri = buildURI(
      { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
      "/music/track1.mp3",
    );
    const result = await Worker.resolve({ uri });
    expect(result).not.toBe(undefined);
    if (result && "url" in result) {
      expect(result.url).toContain("http://127.0.0.1");
      expect(result.url).toContain("/music/track1.mp3");
      expect(result.url).toContain("diffuse%3Abasic-auth=");
      expect(result.expiresAt).toBeGreaterThan(Date.now() / 1000);
    }
  });

  it("resolve returns undefined for a URI without path", async () => {
    const uri = buildURI(
      { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
      "",
    );
    const result = await Worker.resolve({ uri });
    expect(result).toBe(undefined);
  });

  it("groupConsult reports available for a reachable server", async () => {
    const uri = buildURI(
      { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
      "/music/track1.mp3",
    );
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    expect(result[keys[0]].available).toBe(true);
  });

  it("groupConsult reports unavailable for an unreachable server", async () => {
    const uri = buildURI(
      { username: "user", password: "pass", host: `127.0.0.1:${port + 9999}`, dir: "/" },
      "/track.mp3",
    );
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    expect(result[keys[0]].available).toBe(false);
  });

  it("detach with scheme removes all webdav tracks", async () => {
    const tracks: Track[] = [
      {
        $type: "sh.diffuse.output.track",
        id: "1",
        uri: buildURI(
          { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
          "/a.mp3",
        ),
      },
      {
        $type: "sh.diffuse.output.track",
        id: "2",
        uri: buildURI(
          { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
          "/b.mp3",
        ),
      },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "webdav", tracks });
    expect(remaining.length).toBe(0);
  });

  it("detach with a specific server URI removes only that server's tracks", async () => {
    const tracks: Track[] = [
      {
        $type: "sh.diffuse.output.track",
        id: "1",
        uri: buildURI(
          { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
          "/a.mp3",
        ),
      },
      {
        $type: "sh.diffuse.output.track",
        id: "2",
        uri: buildURI(
          { username: "user", password: "pass", host: "other.example.com", dir: "/" },
          "/b.mp3",
        ),
      },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: buildURI(
        { username: "user", password: "pass", host: `127.0.0.1:${port}`, dir: "/" },
        "/a.mp3",
      ),
      tracks,
    });
    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });
});
