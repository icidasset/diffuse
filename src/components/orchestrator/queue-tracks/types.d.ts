import type { Track } from "@definitions/types.d.ts";

export type Actions = {
  poolAvailable(args: {
    ports: { input: MessagePort; queue: MessagePort };
    tracks: Track[];
  }): Promise<void>;
};
