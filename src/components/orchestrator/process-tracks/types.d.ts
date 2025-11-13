import type { Track } from "@common/types.d.ts";

export type Actions = {
  process: (
    args: {
      ports: { input: MessagePort; metadataProcessor: MessagePort };
      tracks: Track[];
    },
  ) => Promise<Track[] | null>;
};

export type ActionsProxied = Actions;
