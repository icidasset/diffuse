import type { Track } from "~/definitions/types.d.ts";

export type Progress = {
  processed: number;
  total: number;
};

export type Actions = {
  process: (args: { tracks: Track[]; disabledUris: string[] }) => Promise<Track[] | null>;
  progress: () => Progress;
};
