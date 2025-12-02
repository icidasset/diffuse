import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  poolAvailable(tracks: Track[]): Promise<void>;
};
