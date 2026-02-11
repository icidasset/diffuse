import type { Signal, SignalReader } from "@common/signal.d.ts";
import type { DiffuseElement } from "@common/element.js";
import type { Facet, Track } from "@definitions/types.d.ts";

export type OutputElement<Encoding = null> =
  & DiffuseElement
  & OutputManagerDeputy<Encoding>;

export type OutputManagerDeputy<Encoding = null> = Omit<
  OutputManager<Encoding>,
  "signals"
>;

export type OutputManager<Encoding = null> = {
  facets: {
    collection: SignalReader<Encoding extends null ? Facet[] : Encoding>;
    reload: () => Promise<void>;
    save: (
      facets: Encoding extends null ? Facet[] : Encoding,
    ) => Promise<void>;
    state: SignalReader<"loading" | "loaded" | "sleeping">;
  };
  signals: {
    facets: Signal<Encoding extends null ? Facet[] : Encoding>;
    tracks: Signal<Encoding extends null ? Track[] : Encoding>;
  };
  tracks: {
    collection: SignalReader<Encoding extends null ? Track[] : Encoding>;
    reload: () => Promise<void>;
    save: (tracks: Encoding extends null ? Track[] : Encoding) => Promise<void>;
    state: SignalReader<"loading" | "loaded" | "sleeping">;
  };
};

export type OutputManagerProperties<Encoding = null> = {
  facets: {
    empty(): Encoding extends null ? Facet[] : Encoding;
    get(): Promise<Encoding extends null ? Facet[] : Encoding>;
    put(
      facets: Encoding extends null ? Facet[] : Encoding,
    ): Promise<void>;
  };
  init?: () => Promise<boolean>;
  tracks: {
    empty(): Encoding extends null ? Track[] : Encoding;
    get(): Promise<Encoding extends null ? Track[] : Encoding>;
    put(tracks: Encoding extends null ? Track[] : Encoding): Promise<void>;
  };
};

export type OutputWorkerActions<DataType> = {
  get(args: { name: string }): Promise<DataType>;
  put(args: { data: DataType; name: string }): Promise<void>;
};
