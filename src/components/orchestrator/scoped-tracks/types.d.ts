import type { SearchParams } from "@orama/orama";

import type { Playlist, Track } from "@definitions/types.d.ts";
import type { Schema } from "@components/processor/search/types.d.ts";

export type Actions = {
  filterByPlaylist(args: { tracks: Track[]; playlist: Playlist }): Promise<Track[]>;
  searchTracks(params: SearchParams<Schema>): Promise<Track[]>;
  supplyAvailable(tracks: Track[]): Promise<void>;
};
