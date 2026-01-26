/**
 * @import { Track } from "@definitions/types.d.ts";
 */

/**
 * @type {Track}
 */
export const trackA = {
  $type: "sh.diffuse.output.tracks",
  id: "sample-a",
  uri: "http://example.com/audio-a.mp3",
  tags: {
    artist: "Artist",
    title: "Sample",
  },
};

/**
 * @type {Track}
 */
export const trackB = {
  $type: "sh.diffuse.output.tracks",
  id: "sample-b",
  uri: "http://example.com/audio-b.mp3",
  tags: {
    album: "B-side",
    title: "Unknown",
  },
};

export const tracks = [
  trackA,
  trackB,
];
