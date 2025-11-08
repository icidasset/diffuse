import type { mainSchema as Track } from "../definitions/types/sh/diffuse/output/tracks.ts";

// RE-EXPORT

export type { mainSchema as Track } from "../definitions/types/sh/diffuse/output/tracks.ts";

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

/* TRACKS */

export type ResolvedUri = undefined | { url: string; expiresAt: number }; // TODO: Streams?
