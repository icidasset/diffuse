import type {
  Collaboration,
  Facet,
  PlaylistItem,
  Setting,
  Track,
} from "~/definitions/types.d.ts";

/**
 * The document shape for every collection.
 *
 * Records are keyed by `id` rather than held in an array so that concurrent
 * edits to different records merge independently; `order` keeps the array order
 * the output interface expects.
 */
export type CollectionDocument<T> = {
  /** The lexicon NSID the records conform to, so the stored binary is self-describing. */
  $schema?: string;
  order: string[];
  records: Record<string, T>;
};

export type CollaborationsDocument = CollectionDocument<Collaboration>;
export type FacetsDocument = CollectionDocument<Facet>;
export type PlaylistItemsDocument = CollectionDocument<PlaylistItem>;
export type SettingsDocument = CollectionDocument<Setting>;
export type TracksDocument = CollectionDocument<Track>;
