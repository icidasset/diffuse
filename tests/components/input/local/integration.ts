import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * Integration tests for the local input that verify the File System handle
 * lifecycle (store → list → resolve → detach) against a real browser.
 *
 * These run in the browser via {@link testWeb} because the worker relies on
 * IndexedDB (via idb-keyval), FileSystemHandles and `URL.createObjectURL`,
 * which are only available in a DOM context. Handles are created using the
 * Origin Private File System (`navigator.storage.getDirectory()`), which
 * yields real, structured-cloneable `FileSystemDirectoryHandle`s — the same
 * kind of handles a user picking a directory would produce.
 */
describe("components/input/local (integration)", () => {
  it("list enumerates audio files from a stored directory handle, preserving cached metadata", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      // Real OPFS tree: tid-aaa/music/{song.mp3, cover.jpg, notes.txt} and
      // tid-aaa/music/nested/song.flac.
      const storage = await navigator.storage.getDirectory();
      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      const music = await library.getDirectoryHandle("music", { create: true });
      const nested = await music.getDirectoryHandle("nested", { create: true });

      const a = await music.getFileHandle("song.mp3", { create: true });
      const aw = await a.createWritable();
      await aw.write(new TextEncoder().encode("audio-bytes-1"));
      await aw.close();

      const b = await music.getFileHandle("cover.jpg", { create: true });
      const bw = await b.createWritable();
      await bw.write(new TextEncoder().encode("image-bytes"));
      await bw.close();

      const c = await music.getFileHandle("notes.txt", { create: true });
      const cw = await c.createWritable();
      await cw.write(new TextEncoder().encode("not audio"));
      await cw.close();

      const d = await nested.getFileHandle("song.flac", { create: true });
      const dw = await d.createWritable();
      await dw.write(new TextEncoder().encode("audio-bytes-2"));
      await dw.close();

      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      return await W.list([{
        $type: "sh.diffuse.output.track",
        id: "c1",
        uri: "local://tid-aaa/music/song.mp3",
        stats: { duration: 183000 },
        tags: { title: "Cached Song" },
      }]);
    });

    // Only audio files become tracks; notes.txt and cover.jpg are excluded.
    expect(result.length).toBe(2);
    const uris = result.map((t) => t.uri);
    expect(uris).toContain("local://tid-aaa/music/song.mp3");
    expect(uris).toContain("local://tid-aaa/music/nested/song.flac");
    expect(uris.some((u) => u.includes("notes.txt"))).toBe(false);

    // Cached stats/tags come back on the refreshed track.
    const song = result.find((t) => t.uri.endsWith("song.mp3"));
    expect(song).toBeDefined();
    if (song) {
      expect(song.id).toBe("c1");
      expect(song.stats).toEqual({ duration: 183000 });
      expect(song.tags).toEqual({ title: "Cached Song" });
    }
  });

  it("list returns a placeholder track when the directory has no audio files", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      return await W.list([{
        $type: "sh.diffuse.output.track",
        id: "p1",
        kind: "placeholder",
        uri: "local://tid-aaa/",
      }]);
    });

    expect(result.length).toBe(1);
    expect(result[0].kind).toBe("placeholder");
    expect(result[0].uri).toBe("local://tid-aaa/");
    // The empty-directory placeholder always gets a fresh id.
    expect(typeof result[0].id).toBe("string");
    expect(result[0].id).not.toBe("");
  });

  it("list keeps cached tracks for a TID that has no stored handle", async () => {
    const result = await testWeb(async () => {
      const W = await import("~/components/input/local/worker.js");

      return await W.list([{
        $type: "sh.diffuse.output.track",
        id: "c2",
        uri: "local://tid-unknown/song.mp3",
      }]);
    });

    expect(result.length).toBe(1);
    expect(result[0].id).toBe("c2");
    expect(result[0].uri).toBe("local://tid-unknown/song.mp3");
  });

  it("list and resolve work with a single stored file handle", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const file = await storage.getFileHandle("track.mp3", { create: true });
      const writable = await file.createWritable();
      await writable.write(new TextEncoder().encode("audio-bytes"));
      await writable.close();

      await IDB.set(IDB_HANDLES, { "tid-file": file });

      const listed = await W.list([{
        $type: "sh.diffuse.output.track",
        id: "c3",
        uri: "local://tid-file/",
      }]);
      const resolved = await W.resolve({ uri: "local://tid-file/" });

      return {
        listed,
        blobUrl: resolved && "url" in resolved ? resolved.url : null,
        expiresAtIsInfinity: resolved !== undefined && "expiresAt" in resolved
          ? resolved.expiresAt === Infinity
          : false,
        fetchedText: resolved && "url" in resolved
          ? await fetch(resolved.url).then((r) => r.text())
          : null,
      };
    });

    expect(result.listed.length).toBe(1);
    expect(result.listed[0].id).toBe("c3");
    expect(result.listed[0].uri).toBe("local://tid-file/");

    expect(result.blobUrl).toMatch(/^blob:/);
    expect(result.expiresAtIsInfinity).toBe(true);
    expect(result.fetchedText).toBe("audio-bytes");
  });

  it("resolve creates a fetchable blob URL for a file in a stored directory handle", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      const music = await library.getDirectoryHandle("music", { create: true });
      const a = await music.getFileHandle("a.mp3", { create: true });
      const aw = await a.createWritable();
      await aw.write(new TextEncoder().encode("audio-bytes"));
      await aw.close();

      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      const resolved = await W.resolve({ uri: "local://tid-aaa/music/a.mp3" });
      return {
        blobUrl: resolved && "url" in resolved ? resolved.url : null,
        expiresAtIsInfinity: resolved !== undefined && "expiresAt" in resolved
          ? resolved.expiresAt === Infinity
          : false,
        fetchedText: resolved && "url" in resolved
          ? await fetch(resolved.url).then((r) => r.text())
          : null,
      };
    });

    expect(result.blobUrl).toMatch(/^blob:/);
    expect(result.expiresAtIsInfinity).toBe(true);
    expect(result.fetchedText).toBe("audio-bytes");
  });

  it("resolve returns undefined for the directory URI itself", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      return await W.resolve({ uri: "local://tid-aaa/" });
    });

    expect(result).toBe(undefined);
  });

  it("detach with a specific URI removes that TID's tracks and its stored handle", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const a = await storage.getDirectoryHandle("tid-aaa", { create: true });
      const b = await storage.getDirectoryHandle("tid-bbb", { create: true });
      await IDB.set(IDB_HANDLES, { "tid-aaa": a, "tid-bbb": b });

      const remaining = await W.detach({
        fileUriOrScheme: "local://tid-aaa/track1.mp3",
        tracks: [
          { $type: "sh.diffuse.output.track", id: "1", uri: "local://tid-aaa/track1.mp3" },
          { $type: "sh.diffuse.output.track", id: "2", uri: "local://tid-bbb/track2.mp3" },
        ],
      });
      const handlesAfter = await IDB.get(IDB_HANDLES);

      return {
        remaining,
        remainingTids: Object.keys(handlesAfter ?? {}),
      };
    });

    expect(result.remaining.length).toBe(1);
    expect(result.remaining[0].id).toBe("2");
    // The detached TID's handle is removed from IndexedDB; the other stays.
    expect(result.remainingTids).toEqual(["tid-bbb"]);
  });

  it("detach with the local scheme removes all local tracks", async () => {
    const result = await testWeb(async () => {
      const W = await import("~/components/input/local/worker.js");

      return await W.detach({
        fileUriOrScheme: "local",
        tracks: [
          { $type: "sh.diffuse.output.track", id: "1", uri: "local://tid-aaa/track1.mp3" },
          { $type: "sh.diffuse.output.track", id: "2", uri: "local://tid-bbb/track2.mp3" },
        ],
      });
    });

    expect(result.length).toBe(0);
  });

  it("artwork returns the cover image bytes from the track's directory", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      const music = await library.getDirectoryHandle("music", { create: true });

      const track = await music.getFileHandle("track.mp3", { create: true });
      const tw = await track.createWritable();
      await tw.write(new TextEncoder().encode("audio-bytes"));
      await tw.close();

      const cover = await music.getFileHandle("cover.jpg", { create: true });
      const cw = await cover.createWritable();
      await cw.write(new TextEncoder().encode("image-bytes-cover"));
      await cw.close();

      const folder = await music.getFileHandle("folder.png", { create: true });
      const fw = await folder.createWritable();
      await fw.write(new TextEncoder().encode("image-bytes-folder"));
      await fw.close();

      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      const art = await W.artwork("local://tid-aaa/music/track.mp3");
      return art ? new TextDecoder().decode(art) : null;
    });

    // cover.jpg is preferred over folder.png.
    expect(result).toBe("image-bytes-cover");
  });

  it("artwork returns null when the directory has no images", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      const music = await library.getDirectoryHandle("music", { create: true });
      const track = await music.getFileHandle("track.mp3", { create: true });
      const writable = await track.createWritable();
      await writable.write(new TextEncoder().encode("audio-bytes"));
      await writable.close();

      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      return await W.artwork("local://tid-aaa/music/track.mp3");
    });

    expect(result).toBe(null);
  });

  it("consult reports availability based on the stored handle's permission", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      // The browser needs to report File System Access support for consult
      // to get past its isSupported() check.
      const storage = await navigator.storage.getDirectory();
      globalThis.showDirectoryPicker = async () => storage;

      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      const granted = await W.consult("local://tid-aaa/");
      const unknown = await W.consult("local://tid-unknown/");

      return {
        granted: granted.supported ? granted.consult : "unsupported",
        unknownSupported: unknown.supported,
      };
    });

    // OPFS handles are always readable by default, so consult says "yes".
    expect(result.granted).toBe("yes");
    expect(result.unknownSupported).toBe(false);
  });

  it("groupConsult groups URIs by TID and skips unknown TIDs", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { IDB_HANDLES } = await import("~/components/input/local/constants.js");
      const W = await import("~/components/input/local/worker.js");

      const storage = await navigator.storage.getDirectory();
      globalThis.showDirectoryPicker = async () => storage;

      const library = await storage.getDirectoryHandle("tid-aaa", { create: true });
      await IDB.set(IDB_HANDLES, { "tid-aaa": library });

      return await W.groupConsult([
        "local://tid-aaa/a.mp3",
        "local://tid-aaa/b.mp3",
        "local://tid-unknown/x.mp3",
      ]);
    });

    expect(Object.keys(result)).toEqual(["local://tid-aaa"]);
    expect(result["local://tid-aaa"].available).toBe("yes");
    expect(result["local://tid-aaa"].scheme).toBe("local");
    expect(result["local://tid-aaa"].uris).toEqual([
      "local://tid-aaa/a.mp3",
      "local://tid-aaa/b.mp3",
    ]);
  });
});