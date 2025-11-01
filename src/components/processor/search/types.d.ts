import type { Track } from "@components/core/types.d.ts";

export type Actions = {
  search(term: string): Promise<Track[]>;
  supply(tracks: Track[]): Promise<void>;
};
