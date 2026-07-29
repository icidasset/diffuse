import { describe, it, beforeAll, afterAll } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { mockServer } from "@tests/common/server.ts";
import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/dropbox/worker.js";
import { buildURI, parseURI } from "~/components/input/dropbox/common.js";

// Mock Dropbox API server.
// The dropbox worker hardcodes `https://api.dropboxapi.com/...` URLs, so we
// monkey-patch `globalThis.fetch` before each test to redirect those calls
// to our in-process mock server.
//
// With the refresh-token flow, every API call first exchanges the refresh
// token for a short-lived access token via POST /oauth2/token. The mock
// server returns `access-token` for `valid-refresh-token` and 401 for
// everything else, then validates `Bearer access-token` on the API calls.

const FILES = [
  { ".tag": "file", name: "song1.mp3", path_lower: "/music/song1.mp3" },
  { ".tag": "file", name: "song2.flac", path_lower: "/music/song2.flac" },
  { ".tag": "file", name: "readme.txt", path_lower: "/readme.txt" },
  { ".tag": "folder", name: "music", path_lower: "/music" },
];

let server: Deno.HttpServer;
let port: number;
let originalFetch: typeof globalThis.fetch;

beforeAll(async () => {
  const started = await mockServer((req, url) => {
    const p = url.pathname;

    // POST /oauth2/token — exchange refresh token for access token
    if (p === "/oauth2/token" && req.method === "POST") {
      return req.text().then((text) => {
        const params = new URLSearchParams(text);
        const grantType = params.get("grant_type");
        const refreshToken = params.get("refresh_token") ?? params.get("code");

        if (grantType === "refresh_token" && refreshToken === "valid-refresh-token") {
          return new Response(JSON.stringify({
            access_token: "access-token",
            expires_in: 14400,
            token_type: "bearer",
          }), { status: 200, headers: { "content-type": "application/json" } });
        }

        // Code exchange (used by exchangeCode, not directly by the worker)
        if (grantType === "authorization_code" && refreshToken === "valid-code") {
          return new Response(JSON.stringify({
            access_token: "access-token",
            refresh_token: "valid-refresh-token",
            expires_in: 14400,
            token_type: "bearer",
          }), { status: 200, headers: { "content-type": "application/json" } });
        }

        return new Response(JSON.stringify({ error: "invalid_grant" }), {
          status: 400,
          headers: { "content-type": "application/json" },
        });
      });
    }

    // GET /2/users/get_current_account — validates the access token
    if (p === "/2/users/get_current_account") {
      const auth = req.headers.get("authorization");
      if (auth === "Bearer access-token") {
        return new Response(JSON.stringify({ account_id: "dbid:123" }), {
          status: 200,
          headers: { "content-type": "application/json" },
        });
      }
      return new Response("", { status: 401 });
    }

    // POST /2/files/list_folder — list files in a directory
    if (p === "/2/files/list_folder") {
      const auth = req.headers.get("authorization");
      if (auth !== "Bearer access-token") {
        return new Response("", { status: 401 });
      }
      return new Response(
        JSON.stringify({
          entries: FILES,
          has_more: false,
          cursor: null,
        }),
        { status: 200, headers: { "content-type": "application/json" } },
      );
    }

    // POST /2/files/list_folder/continue — paginate
    if (p === "/2/files/list_folder/continue") {
      return new Response(
        JSON.stringify({ entries: [], has_more: false, cursor: null }),
        { status: 200, headers: { "content-type": "application/json" } },
      );
    }

    // POST /2/files/get_temporary_link — get a temporary download link
    if (p === "/2/files/get_temporary_link") {
      const auth = req.headers.get("authorization");
      if (auth !== "Bearer access-token") {
        return new Response("", { status: 401 });
      }
      return new Response(
        JSON.stringify({
          link: `http://127.0.0.1:${port}/dl/temp-link`,
          metadata: { name: "song1.mp3" },
        }),
        { status: 200, headers: { "content-type": "application/json" } },
      );
    }

    // The temporary download link itself
    if (p === "/dl/temp-link") {
      return new Response(new Uint8Array(64), {
        status: 200,
        headers: { "content-type": "audio/mpeg" },
      });
    }

    return new Response("", { status: 404 });
  });
  server = started.server;
  port = started.port;

  // Monkey-patch fetch to redirect Dropbox API calls to our mock.
  originalFetch = globalThis.fetch;
  globalThis.fetch = ((input: string | URL | Request, init?: RequestInit) => {
    const url = typeof input === "string" ? input : input instanceof URL ? input.href : input.url;
    const redirected = url
      .replace("https://api.dropboxapi.com", `http://127.0.0.1:${port}`)
      .replace("https://content.dropboxapi.com", `http://127.0.0.1:${port}`);
    return originalFetch(redirected, init);
  }) as typeof globalThis.fetch;
});

afterAll(async () => {
  globalThis.fetch = originalFetch;
  await server.shutdown();
});

describe("components/input/dropbox (integration)", () => {
  it("consult returns true for a valid refresh token", async () => {
    const uri = buildURI({ refreshToken: "valid-refresh-token", directoryPath: "/" }, "/");
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("yes");
    }
  });

  it("consult returns false for an invalid refresh token", async () => {
    const uri = buildURI({ refreshToken: "invalid-refresh-token", directoryPath: "/" }, "/");
    const result = await Worker.consult(uri);
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("no");
    }
  });

  it("list returns audio files from Dropbox", async () => {
    const account = { refreshToken: "valid-refresh-token", directoryPath: "/" };
    const uri = buildURI(account, "/");
    const tracks = await Worker.list([{
      $type: "sh.diffuse.output.track",
      id: "p1",
      kind: "placeholder",
      uri,
    }]);

    // Should find song1.mp3 and song2.flac; readme.txt is not audio
    expect(tracks.length).toBe(2);
    const paths = tracks.map((t) => {
      const parsed = parseURI(t.uri);
      return parsed?.path;
    });
    expect(paths).toContain("/music/song1.mp3");
    expect(paths).toContain("/music/song2.flac");
  });

  it("list returns placeholder when API returns error", async () => {
    const account = { refreshToken: "bad-refresh-token", directoryPath: "/" };
    const uri = buildURI(account, "/");
    const tracks = await Worker.list([{
      $type: "sh.diffuse.output.track",
      id: "p1",
      kind: "placeholder",
      uri,
    }]);

    // listFiles returns null on error, so worker returns a placeholder
    expect(tracks.length).toBe(1);
    expect(tracks[0].kind).toBe("placeholder");
  });

  it("resolve returns a temporary link URL", async () => {
    const uri = buildURI(
      { refreshToken: "valid-refresh-token", directoryPath: "/" },
      "/music/song1.mp3",
    );
    const result = await Worker.resolve({ uri });
    expect(result).not.toBe(undefined);
    if (result && "url" in result) {
      expect(result.url).toContain("temp-link");
      // Dropbox temporary links expire after 4 hours
      const fourHours = 4 * 60 * 60;
      const now = Math.round(Date.now() / 1000);
      expect(result.expiresAt).toBeGreaterThan(now);
      expect(result.expiresAt).toBeLessThanOrEqual(now + fourHours + 10);
    }
  });

  it("resolve returns undefined for root path", async () => {
    const uri = buildURI(
      { refreshToken: "valid-refresh-token", directoryPath: "/" },
      "/",
    );
    const result = await Worker.resolve({ uri });
    expect(result).toBe(undefined);
  });

  it("groupConsult reports available for a valid token", async () => {
    const uri = buildURI(
      { refreshToken: "valid-refresh-token", directoryPath: "/" },
      "/music/song1.mp3",
    );
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    expect(result[keys[0]].available).toBe("yes");
  });

  it("groupConsult reports unavailable for an invalid token", async () => {
    const uri = buildURI(
      { refreshToken: "invalid-refresh-token", directoryPath: "/" },
      "/music/song1.mp3",
    );
    const result = await Worker.groupConsult([uri]);
    const keys = Object.keys(result);
    expect(keys.length).toBe(1);
    expect(result[keys[0]].available).toBe("no");
  });

  it("detach with scheme removes all dropbox tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: buildURI({ refreshToken: "t1", directoryPath: "/" }, "/a.mp3") },
      { $type: "sh.diffuse.output.track", id: "2", uri: buildURI({ refreshToken: "t1", directoryPath: "/" }, "/b.mp3") },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "dropbox", tracks });
    expect(remaining.length).toBe(0);
  });

  it("detach with a specific account URI removes only that account's tracks", async () => {
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: buildURI({ refreshToken: "token-a", directoryPath: "/" }, "/a.mp3") },
      { $type: "sh.diffuse.output.track", id: "2", uri: buildURI({ refreshToken: "token-b", directoryPath: "/" }, "/b.mp3") },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: buildURI({ refreshToken: "token-a", directoryPath: "/" }, "/a.mp3"),
      tracks,
    });
    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });
});
