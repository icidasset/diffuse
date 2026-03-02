import type { TrackTags } from "~/definitions/types.d.ts";

export type Actions = {
  artwork(request: ArtworkRequest): Promise<Artwork[]>;
  supply(items: ArtworkRequest[]): void;
};

export type Artwork = {
  bytes: Uint8Array;
  mime: string;
};

export type ArtworkRequest<Tags = TrackTags> = {
  cacheId: string;
  mimeType?: string;
  stream?: ReadableStream;
  tags?: Tags;
  urls?: Urls;
  variousArtists?: boolean;
};

// export type State = {
//   artwork: Record<string, Artwork[]>;
// };

export type Urls = { get: string; head: string };
