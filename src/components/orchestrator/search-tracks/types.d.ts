import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  supplyAvailable(args: {
    ports: { input: MessagePort; search: MessagePort };
    tracks: Track[];
  }): Promise<void>;
};
