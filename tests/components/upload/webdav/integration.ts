import { afterAll, beforeAll, describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import type { Server } from "@specs/components/input/webdav/types.d.ts";
import * as Worker from "~/components/upload/webdav/worker.js";
import { buildURI } from "~/components/input/webdav/common.js";

/**
 * Integration tests for the webdav upload worker that verify its WebDAV API
 * usage (upload, delete, consult) against an in-memory mock WebDAV endpoint.
 *
 * The worker performs its HTTP requests using the global `fetch`. We
 * intercept `fetch` and respond with WebDAV responses (`multistatus` for
 * PROPFIND), so no real network, servers or credentials are involved.
 */

type StoredFile = { path: string; bytes: Uint8Array<ArrayBuffer> };

type MockServer = {
  host: string;
  files: StoredFile[];
  /** When set, the mock endpoint fails all requests (transient network blip). */
  failRequests?: boolean;
};

const mockServers: MockServer[] = [];

function multistatusXml(hrefs: string[]): string {
  const responses = hrefs.map((href) =>
    `  <d:response>
    <d:href>${href}</d:href>
    <d:propstat>
      <d:prop><d:resourcetype/></d:prop>
      <d:status>HTTP/1.1 200 OK</d:status>
    </d:propstat>
  </d:response>`
  ).join("\n");

  return `<?xml version="1.0" encoding="UTF-8"?>
<d:multistatus xmlns:d="DAV:">
${responses}
</d:multistatus>`;
}

function registerMockServer(server: Server, files: StoredFile[]): MockServer {
  const mock: MockServer = { host: server.host, files };
  mockServers.push(mock);
  return mock;
}

function makeServer(host: string, dir = "/"): Server {
  return {
    username: "user",
    password: "pass",
    // Unique host per server: the consult cache is keyed by the server id,
    // so shared hosts would leak cached availability between tests.
    host,
    dir,
  };
}

function makeFile(name: string, contents: string, type = "audio/mpeg"): File {
  return new File([new TextEncoder().encode(contents)], name, { type });
}

let originalFetch: typeof globalThis.fetch;

beforeAll(() => {
  originalFetch = globalThis.fetch;

  globalThis.fetch =
    (async (input: string | URL | Request, init?: RequestInit) => {
      const url = new URL(
        typeof input === "string"
          ? input
          : input instanceof URL
          ? input.href
          : input.url,
      );

      const mock = mockServers.find((s) => url.hostname === s.host);

      if (!mock) {
        return new Response(null, { status: 404 });
      }

      if (mock.failRequests) {
        throw new TypeError("fetch failed");
      }

      const method = init?.method ?? "GET";
      const path = url.pathname;

      // PROPFIND — used by consult (Depth: 0 reachability check).
      if (method === "PROPFIND") {
        return new Response(multistatusXml([path]), {
          status: 207,
          headers: { "content-type": "application/xml" },
        });
      }

      // PUT — store the body under the path.
      if (method === "PUT") {
        const bytes = new Uint8Array(
          await new Response(init?.body).arrayBuffer(),
        );
        const existing = mock.files.findIndex((f) => f.path === path);
        const stored = { path, bytes };
        if (existing >= 0) mock.files[existing] = stored;
        else mock.files.push(stored);
        return new Response(null, { status: 201 });
      }

      // DELETE — remove the file under the path.
      if (method === "DELETE") {
        const existing = mock.files.findIndex((f) => f.path === path);
        if (existing >= 0) mock.files.splice(existing, 1);
        return new Response(null, { status: 204 });
      }

      return new Response(null, { status: 405 });
    }) as typeof globalThis.fetch;
});

afterAll(() => {
  globalThis.fetch = originalFetch;
});

describe("components/upload/webdav (integration)", () => {
  it("upload stores the file under the server's directory and returns a parseable URI", async () => {
    const server = makeServer("upload-dir.example.com", "/Music");
    const mock = registerMockServer(server, []);

    const file = makeFile("my song.mp3", "fake-audio-bytes");
    const fileUri = await Worker.upload({ file, uri: buildURI(server) });

    // The stored path is the server directory plus the file name; the
    // returned URI is the canonical (percent-encoded) form — matching what
    // the matching input component's listing produces.
    expect(mock.files.length).toBe(1);
    expect(mock.files[0].path).toBe("/Music/my%20song.mp3");
    expect(new TextDecoder().decode(mock.files[0].bytes)).toBe(
      "fake-audio-bytes",
    );

    const parsed = buildURI(server, "/Music/my%20song.mp3");
    expect(fileUri).toBe(parsed);
  });

  it("upload places files at the root when no directory is configured", async () => {
    const server = makeServer("upload-root.example.com");
    const mock = registerMockServer(server, []);

    const file = makeFile("bafkqaaa.flac", "fake-audio-bytes", "audio/flac");
    const fileUri = await Worker.upload({ file, uri: buildURI(server) });

    expect(mock.files.length).toBe(1);
    expect(mock.files[0].path).toBe("/bafkqaaa.flac");
    expect(fileUri).toBe(buildURI(server, "/bafkqaaa.flac"));
  });

  it("upload overwrites an existing file at the same path (idempotent re-upload)", async () => {
    const server = makeServer("upload-overwrite.example.com");
    const mock = registerMockServer(server, []);

    await Worker.upload({
      file: makeFile("bafkqaaa.mp3", "first-version"),
      uri: buildURI(server),
    });
    await Worker.upload({
      file: makeFile("bafkqaaa.mp3", "second-version"),
      uri: buildURI(server),
    });

    expect(mock.files.length).toBe(1);
    expect(new TextDecoder().decode(mock.files[0].bytes)).toBe(
      "second-version",
    );
  });

  it("upload throws a descriptive error when the server is unreachable", async () => {
    const server = makeServer("upload-failing.example.com");
    const mock = registerMockServer(server, []);
    mock.failRequests = true;

    await expect(Worker.upload({
      file: makeFile("bafkqaaa.mp3", "fake-audio-bytes"),
      uri: buildURI(server),
    })).rejects.toThrow(
      /Failed to upload "bafkqaaa.mp3" to WebDAV server "upload-failing.example.com"/,
    );
  });

  it("delete removes the file via the URI produced by upload", async () => {
    const server = makeServer("delete.example.com", "/Music");
    const mock = registerMockServer(server, [
      { path: "/Music/my%20song.mp3", bytes: new Uint8Array(1) },
    ]);

    await Worker.deleteFn(buildURI(server, "/Music/my%20song.mp3"));

    expect(mock.files.length).toBe(0);
  });

  it("delete throws for an account URI without a file path", async () => {
    const server = makeServer("delete-account.example.com");
    registerMockServer(server, []);

    await expect(Worker.deleteFn(buildURI(server))).rejects.toThrow(
      /Invalid WebDAV file URI/,
    );
  });

  it("delete throws for an unparseable URI", async () => {
    await expect(Worker.deleteFn("webdav://")).rejects.toThrow(
      /Invalid WebDAV URI/,
    );
  });

  it("consult returns yes for a reachable server", async () => {
    const server = makeServer("consult-yes.example.com");
    registerMockServer(server, []);

    const result = await Worker.consult(buildURI(server));
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("yes");
    }
  });

  it("consult returns no for a server that does not exist", async () => {
    const server = makeServer("consult-no.example.com");
    // No server registered — the mock endpoint answers 404.

    const result = await Worker.consult(buildURI(server));
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("no");
    }
  });

  it("consult returns undetermined for scheme only and invalid URIs", async () => {
    const schemeResult = await Worker.consult("webdav");
    expect(schemeResult.supported).toBe(true);
    if (schemeResult.supported) {
      expect(schemeResult.consult).toBe("undetermined");
    }

    const invalidResult = await Worker.consult("webdav://");
    expect(invalidResult.supported).toBe(true);
    if (invalidResult.supported) {
      expect(invalidResult.consult).toBe("undetermined");
    }
  });

  it("createSource is rejected — WebDAV servers are connected via the Connect page", async () => {
    await expect(Worker.createSource({
      scheme: "webdav",
      refreshToken: "irrelevant",
      directoryPath: "/Diffuse",
    })).rejects.toThrow(/Connect page/);
  });
});
