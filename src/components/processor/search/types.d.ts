import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  search(term: string): Promise<Track[]>;
  supply(tracks: Track[]): Promise<void>;
};
