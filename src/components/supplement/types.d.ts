import type { DiffuseElement } from "~/common/element.js";
import type { SignalReader } from "~/common/signal.d.ts";
import type { Track } from "~/definitions/types.d.ts";

export type ScrobbleElement = DiffuseElement & ScrobbleActions & {
  isAuthenticated: SignalReader<boolean>;
  handle: SignalReader<string | null>;
};

export type ScrobbleActions = {
  nowPlaying(track: Track): Promise<unknown>;

  /**
   * @param {Track} track
   * @param {number} startedAt Unix timestamp in milliseconds
   */
  scrobble(track: Track, startedAt: number): Promise<unknown>;
};
