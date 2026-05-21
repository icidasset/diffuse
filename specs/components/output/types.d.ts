import type { Signal, SignalReader } from "~/common/signal.d.ts";
import type { DiffuseElement } from "~/common/element.js";
import type {
  Facet,
  PlaylistItem,
  Setting,
  Track,
} from "~/definitions/types.d.ts";

export type OutputElement<Encoding = null> =
  & DiffuseElement
  & OutputManagerDeputy<Encoding>;

export type OutputManagerDeputy<Encoding = null> =
  & Omit<OutputManager<Encoding>, "signals">
  & { ready: SignalReader<boolean> };

export type OutputManager<Encoding = null> = {
  facets: {
    collection: SignalReader<
      | { state: "loading" }
      | { state: "loaded"; data: Encoding extends null ? Facet[] : Encoding }
    >;
    reload: () => Promise<void>;
    save: (
      facets: Encoding extends null ? Facet[] : Encoding,
    ) => Promise<void>;
  };
  playlistItems: {
    collection: SignalReader<
      | { state: "loading" }
      | { state: "loaded"; data: Encoding extends null ? PlaylistItem[] : Encoding }
    >;
    reload: () => Promise<void>;
    save: (
      playlistItems: Encoding extends null ? PlaylistItem[] : Encoding,
    ) => Promise<void>;
  };
  settings: {
    collection: SignalReader<
      | { state: "loading" }
      | { state: "loaded"; data: Encoding extends null ? Setting[] : Encoding }
    >;
    reload: () => Promise<void>;
    save: (
      settings: Encoding extends null ? Setting[] : Encoding,
    ) => Promise<void>;
  };
  signals: {
    facets: Signal<Encoding extends null ? Facet[] : Encoding>;
    playlistItems: Signal<Encoding extends null ? PlaylistItem[] : Encoding>;
    settings: Signal<Encoding extends null ? Setting[] : Encoding>;
    tracks: Signal<Encoding extends null ? Track[] : Encoding>;
  };
  tracks: {
    collection: SignalReader<
      | { state: "loading" }
      | { state: "loaded"; data: Encoding extends null ? Track[] : Encoding }
    >;
    reload: () => Promise<void>;
    save: (tracks: Encoding extends null ? Track[] : Encoding) => Promise<void>;
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
  playlistItems: {
    empty(): Encoding extends null ? PlaylistItem[] : Encoding;
    get(): Promise<Encoding extends null ? PlaylistItem[] : Encoding>;
    put(
      playlistItems: Encoding extends null ? PlaylistItem[] : Encoding,
    ): Promise<void>;
  };
  settings: {
    empty(): Encoding extends null ? Setting[] : Encoding;
    get(): Promise<Encoding extends null ? Setting[] : Encoding>;
    put(
      settings: Encoding extends null ? Setting[] : Encoding,
    ): Promise<void>;
  };
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
