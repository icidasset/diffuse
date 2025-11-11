import type { SignalReader } from "@common/signal.d.ts";

export type OutputElement<Tracks> = HTMLElement & OutputManager<Tracks>;

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
