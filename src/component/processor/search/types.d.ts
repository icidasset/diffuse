import type { Track } from "@component/core/types.d.ts";

export type Actions = {
  search(term: string): Promise<Track[]>;
  supply(tracks: Track[]): Promise<void>;
};
