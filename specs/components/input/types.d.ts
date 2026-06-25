import type { ProxiedActions } from "~/common/worker.d.ts";

import type { Track } from "~/definitions/types.d.ts";
import type { DiffuseElement } from "~/common/element.js";

/**
 * Consultation.
 *
 * `consult` can be "undetermined" if only a scheme was given
 * instead of a full URI.
 */
export type Consult =
  | { supported: false; reason: string }
  | { supported: true; consult: "undetermined" | ConsultResult };

/**
 * Tri-state availability for a single source.
 *
 * - `"yes"`    → the source confirmed it is reachable;
 * - `"no"`     → the source explicitly rejected (e.g. HTTP 404, auth fail);
 * - `"unsure"` → the consult was inconclusive (network blip, timeout,
 *                aborted fetch). Consult results are never cached in
 *                this state, and consumers should treat the source
 *                optimistically rather than hiding it.
 */
export type ConsultResult = "yes" | "no" | "unsure";

export type ConsultGrouping = {
  available: ConsultResult;
  reason?: string;
  scheme: string;
  uris: string[];
};

export type GroupConsult = Record<string, ConsultGrouping>;

export type InputActions = {
  artwork(uri: string): Promise<Uint8Array | null>;
  consult(uriOrScheme: string): Promise<Consult>;
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

export type ResolvedUri = undefined | ResolveUriAsUrl | ResolveUriAsStream;

export type ResolveUriAsUrl = {
  expiresAt: number;
  url: string;
};

export type ResolveUriAsStream = {
  expiresAt: number;
  mimeType: string;
  stream: ReadableStream;

  /** Total duration in seconds. */
  duration?: number;
};

export type Source = { label: string; uri: string };
