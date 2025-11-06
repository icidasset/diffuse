import type { SignalReader } from "@common/signal.d.ts";
import type { Track } from "@common/types.d.ts";

// TODO: Do we need this?
//
// export interface Output<S = TrackStats, T = TrackTags> {
//   tracks: Track<S, T>[];
// }

export type OutputElement = HTMLElement & OutputManager<Track[]>;

export type OutputManager<Tracks> = {
  tracks: {
    collection: SignalReader<Tracks>;
    reload: () => Promise<void>;
    save: (tracks: Tracks) => Promise<void>;
    state: SignalReader<"loading" | "loaded">;
  };
};

export type OutputManagerProperties<Tracks> = {
  init?: () => Promise<boolean>;
  tracks: {
    empty(): Tracks;
    get(): Promise<Tracks>;
    put(tracks: Tracks): Promise<void>;
  };
};
