import type { Track } from "~/definitions/types.d.ts";

export type Progress = {
  processed: number;
  total: number;
};

export type Actions = {
  process: (args: {
    tracks: Track[];
    disabledUris: string[];
    /** When provided, only tracks whose uri starts with one of these prefixes are processed. */
    onlyUris?: string[];
  }) => Promise<Track[] | null>;
  progress: () => Progress;
};
