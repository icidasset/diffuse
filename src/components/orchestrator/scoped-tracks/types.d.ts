import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  supply(tracks: Track[]): Promise<{ availableTracks: Track[] }>;
};
