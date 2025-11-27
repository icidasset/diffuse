import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  process: (
    args: {
      ports: { input: MessagePort; metadataProcessor: MessagePort };
      tracks: Track[];
    },
  ) => Promise<Track[] | null>;
};
