import type {
  Facet,
  PlaylistItem,
  Setting,
  Track,
} from "~/definitions/types.d.ts";

export type FacetsDocument = { collection: Facet[] };
export type PlaylistItemsDocument = { collection: PlaylistItem[] };
export type SettingsDocument = { collection: Setting[] };
export type TracksDocument = { collection: Track[] };
