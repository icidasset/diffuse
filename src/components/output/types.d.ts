import type { Signal, SignalReader } from "@common/signal.d.ts";
import type { DiffuseElement } from "@common/element.js";

export type OutputElement<Tracks> =
  & DiffuseElement
  & OutputManagerDeputy<Tracks>;

export type OutputManagerDeputy<Tracks> = Omit<
  OutputManager<Tracks>,
  "signals"
>;

export type OutputManager<Tracks> = {
  signals: {
    tracks: Signal<Tracks>;
  };
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
