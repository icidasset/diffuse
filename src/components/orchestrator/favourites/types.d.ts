import type { Playlist, Track } from "@definitions/types.d.ts";

export type Actions = {
  include(args: { playlists: Playlist[]; tracks: Track[] }): Promise<
    Playlist[] | null
  >;
  expel(args: { playlists: Playlist[]; tracks: Track[] }): Promise<
    Playlist[] | null
  >;
  toggle(args: { playlists: Playlist[]; tracks: Track[] }): Promise<
    Playlist[] | null
  >;
};
