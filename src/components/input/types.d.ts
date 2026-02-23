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
  | { available: false; reason: string; scheme: string; uris: string[] }
  | { available: true; scheme: string; uris: string[] };

export type GroupConsult = Record<string, ConsultGrouping>;

export type InputActions = {
  consult(fileUriOrScheme: string): Promise<Consult>;
  detach(args: { fileUriOrScheme: string; tracks: Track[] }): Promise<Track[]>;
  groupConsult(uris: string[]): Promise<GroupConsult>;
  list(tracks: Track[]): Promise<Track[]>;
  resolve(args: { method?: string; uri: string }): Promise<ResolvedUri>;
};

export type InputElement =
  & DiffuseElement
  & InputSchemeProvider
  & ProxiedActions<InputActions>
  & { sources: (tracks: Track[]) => Source[] };

export type InputSchemeProvider = { SCHEME: string };

export type ResolvedUri = undefined | {
  stream: ReadableStream;
  expiresAt: number;
} | { url: string; expiresAt: number };

export type Source = { label: string; uri: string };
