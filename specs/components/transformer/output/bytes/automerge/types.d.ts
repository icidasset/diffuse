import type {
  Facet,
  PlaylistItem,
  Setting,
  Track,
} from "~/definitions/types.d.ts";

/**
 * The schema identity stamped on an Automerge doc so the stored binary is
 * self-describing.
 */
export type DocumentSchema = {
  /** The lexicon NSID the records in `collection` conform to. */
  $schema?: string;
};

export type FacetsDocument = { collection: Facet[] } & DocumentSchema;
export type PlaylistItemsDocument = { collection: PlaylistItem[] } & DocumentSchema;
export type SettingsDocument = { collection: Setting[] } & DocumentSchema;
export type TracksDocument = { collection: Track[] } & DocumentSchema;
