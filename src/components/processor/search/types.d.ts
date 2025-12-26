import type { SignalReader } from "@common/signal.d.ts";
import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  search(term: string): Promise<Track[]>;
  supply(tracks: Track[]): Promise<void>;
};

export type State = {
  cacheId: SignalReader<string>;
};
