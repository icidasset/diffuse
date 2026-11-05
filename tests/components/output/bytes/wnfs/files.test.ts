import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  buildURI,
  mimeType,
  outputChannelName,
  parseURI,
  SCHEME,
  SOURCE_URI,
} from "~/components/output/bytes/wnfs/files.js";

describe("components/output/bytes/wnfs/files", () => {
  it("builds and parses file URIs", () => {
    expect(SCHEME).toBe("wnfs+firkin");
    expect(SOURCE_URI).toBe("wnfs+firkin://me");

    const uri = buildURI("Artist - Title.mp3");
    expect(uri).toBe("wnfs+firkin://me/files/Artist%20-%20Title.mp3");
    expect(parseURI(uri)).toEqual({ name: "Artist - Title.mp3" });
  });

  it("rejects URIs that are not files in this storage", () => {
    expect(parseURI(SOURCE_URI)).toBeNull();
    expect(parseURI("wnfs+firkin://me/other/x.mp3")).toBeNull();
    expect(parseURI("wnfs+firkin://me/files/a/b.mp3")).toBeNull();
    expect(parseURI("s3://bucket/x.mp3")).toBeNull();
    expect(parseURI("wnfs+firkin://me/files/%E0%A4%A")).toBeNull();
  });

  it("names the output broadcast channel like the element identifier", () => {
    expect(outputChannelName({ group: "g", namespace: "n" })).toBe(
      "diffuse/output/bytes/wnfs/g/n",
    );
    expect(outputChannelName({ group: "g" })).toBe(
      "diffuse/output/bytes/wnfs/g",
    );
    expect(outputChannelName({})).toBe("diffuse/output/bytes/wnfs/default");
  });

  it("guesses audio mime types from the file name", () => {
    expect(mimeType("track.MP3")).toBe("audio/mpeg");
    expect(mimeType("track.m4a")).toBe("audio/mp4");
    expect(mimeType("track.flac")).toBe("audio/flac");
    expect(mimeType("track.unknown")).toBe("application/octet-stream");
    expect(mimeType("track")).toBe("application/octet-stream");
  });
});
