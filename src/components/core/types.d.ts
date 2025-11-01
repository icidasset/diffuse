import type { SignalReader } from "@common/signal.d.ts";

/* INPUT */

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

export type InputElement = HTMLElement & InputActions;

/* OUTPUT */

export interface Output<S = TrackStats, T = TrackTags> {
  tracks: Track<S, T>[];
}

export type OutputActions = {
  getTracks(): Promise<Track[]>;
  putTracks(tracks: Track[]): Promise<void>;
};

export type OutputElement = HTMLElement & OutputManager;

export type OutputManager = {
  tracks: {
    collection: SignalReader<Track[]>;
    reload: () => Promise<void>;
    save: (tracks: Track[]) => Promise<void>;
    state: SignalReader<"loading" | "loaded">;
  };
};

/* TRACKS */

export type ResolvedUri = undefined | { url: string; expiresAt: number }; // TODO: Streams?

export interface Track<Stats = TrackStats, Tags = TrackTags> {
  id: string;

  kind?: "music" | "audiobook" | "podcast" | "placeholder" | "miscellaneous";
  stats?: Stats;
  tags?: Tags;

  // NOTE: This is a "semi-permanent" URI.
  //
  // Tracks are cached so you can't, for example,
  // use an URL that expires in several hours.
  uri: string;
}

export interface TrackStats {
  bitrate?: number;
  duration?: number;
}

export interface TrackTags {
  album?: string;
  artist?: string;
  disc: { no: number; of?: number };
  genre?: string;
  title: string;
  track: { no: number; of?: number };
  year?: number;
}
