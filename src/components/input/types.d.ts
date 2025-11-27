import type { ProxiedActions } from "@common/worker.d.ts";

import type { Track } from "@definitions/types.d.ts";
import type { DiffuseElement } from "@common/element.js";

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
    { method, uri }: { method?: string; uri: string },
  ): Promise<ResolvedUri>;
};

export type InputElement =
  & DiffuseElement
  & ProxiedActions<InputActions>;

export type ResolvedUri = undefined | {
  stream: ReadableStream;
  expiresAt: number;
} | { url: string; expiresAt: number };
