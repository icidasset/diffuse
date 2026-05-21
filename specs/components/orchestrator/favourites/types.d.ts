import type { PlaylistItem, Track } from "~/definitions/types.d.ts";

export type Actions = {
  include(args: { playlistItems: PlaylistItem[]; tracks: Track[] }): Promise<
    PlaylistItem[] | null
  >;
  expel(args: { playlistItems: PlaylistItem[]; tracks: Track[] }): Promise<
    PlaylistItem[] | null
  >;
  toggle(args: { playlistItems: PlaylistItem[]; tracks: Track[] }): Promise<
    PlaylistItem[] | null
  >;
};
