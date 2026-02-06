import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  poolAvailable(_: { tracks: Track[] }): Promise<void>;
};
