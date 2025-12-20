import type { SignalReader } from "@common/signal.d.ts";
import type { DiffuseElement } from "@common/element.js";

export type OutputElement<Tracks> = DiffuseElement & OutputManager<Tracks>;

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

export type OutputWorkerActions<DataType> = {
  get(args: { name: string }): Promise<DataType>;
  put(args: { data: DataType; name: string }): Promise<void>;
};
