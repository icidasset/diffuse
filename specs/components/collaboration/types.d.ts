import type { DiffuseElement } from "~/common/element.js";
import type { Collaboration, PlaylistItem } from "~/definitions/types.d.ts";

/**
 * The hint prefix a source handles, e.g. `atproto` for `atproto/handle` and
 * `atproto/did`.
 */
export type CollaborationSourceProvider = { PREFIX: string };

/**
 * Reads collaboration records and playlist items from another account.
 */
export type CollaborationSourceActions = {
  loadCollaborations(actor: string): Promise<Collaboration[]>;
  loadPlaylistItems(
    actor: string,
    playlist: string,
  ): Promise<{ owner: string; items: PlaylistItem[] }>;
};

export type CollaborationSourceElement =
  & DiffuseElement
  & CollaborationSourceProvider
  & CollaborationSourceActions;
