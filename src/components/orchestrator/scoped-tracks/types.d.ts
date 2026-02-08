import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  supplyAvailable(tracks: Track[]): Promise<void>;
};
