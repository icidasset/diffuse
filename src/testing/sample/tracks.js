/**
 * @import { Track } from "@definitions/types.d.ts";
 */

/**
 * @type {Track}
 */
const trackA = {
  $type: "sh.diffuse.output.tracks",
  id: "sample-a",
  uri: "http://example.com/audio-a.mp3",
};

/**
 * @type {Track}
 */
const trackB = {
  $type: "sh.diffuse.output.tracks",
  id: "sample-b",
  uri: "http://example.com/audio-b.mp3",
};

export const tracks = [
  trackA,
  trackB,
];
