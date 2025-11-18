import type { Track } from "@definitions/types.d.ts";
import type {
  PortProviderMethod,
  ProxiedActions,
  ProxyProviderMethod,
  WorkerProviderMethod,
} from "./worker.d.ts";

// RE-EXPORT

export type { Track, TrackStats, TrackTags } from "@definitions/types.d.ts";

// INPUT

/**
 * Consultation.
 *
 * `consult` can be "undetermined" if only a scheme was given
 * instead of a full URI.
 */
export type Consult =
  | { supported: false; reason: string }
  | { supported: true; consult: "undetermined" | boolean };

export type ConsultGrouping =
  | { available: false; reason: string; tracks: Track[] }
  | { available: true; tracks: Track[] };

export type GroupConsult = Record<string, ConsultGrouping>;

export type InputActions = {
  consult(fileUriOrScheme: string): Promise<Consult>;
  contextualize(tracks: Track[]): Promise<void>;
  groupConsult(tracks: Track[]): Promise<GroupConsult>;
  list(cachedTracks: Track[]): Promise<Track[]>;
  resolve(
    { method, uri }: { method: string; uri: string },
  ): Promise<ResolvedUri>;
};

export type InputElement =
  & HTMLElement
  & WorkerProviderMethod
  & ProxiedActions<InputActions>
  & ProxyProviderMethod<InputActions>;

// MISC

export type IncompleteArray<T> = ["Missing required items", T];

// TRACKS

export type ResolvedUri = undefined | {
  stream: ReadableStream;
  expiresAt: number;
} | { url: string; expiresAt: number };
