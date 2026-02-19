import type { SearchParams } from "@orama/orama";

import type { PlaylistItem, Track } from "@definitions/types.d.ts";
import type { Schema } from "@components/processor/search/types.d.ts";

export type Actions = {
  filterByPlaylist(
    args: { tracks: Track[]; playlistItems: PlaylistItem[] },
  ): Promise<Track[]>;
  searchTracks(params: SearchParams<Schema>): Promise<Track[]>;
  supply(tracks: Track[]): Promise<{ availableTracks: Track[] }>;
};
