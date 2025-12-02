import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  process: (tracks: Track[]) => Promise<Track[] | null>;
};
