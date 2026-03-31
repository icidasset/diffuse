/**
 * @import { Track } from "~/definitions/types.d.ts";
 */

/**
 * @type {Track}
 */
export const trackA = {
  $type: "sh.diffuse.output.track",
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
  $type: "sh.diffuse.output.track",
  id: "sample-b",
  uri: "http://example.com/audio-b.mp3",
  tags: {
    album: "B-side",
    title: "Unknown",
  },
};

/**
 * @type {Track}
 */
export const trackC = {
  $type: "sh.diffuse.output.track",
  id: "sample-c",
  uri: "http://example.com/audio-c.mp3",
  tags: {
    title: "Sample C",
  },
};

export const tracks = [
  trackA,
  trackB,
  trackC,
];
