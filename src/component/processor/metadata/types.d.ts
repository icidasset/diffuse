import type { IPicture } from "music-metadata";
import type { TrackStats, TrackTags } from "@component/core/types.d.ts";

export type Actions = {
  supply: (
    args: {
      includeArtwork?: boolean;
      mimeType?: string;
      stream?: ReadableStream;
      urls?: Urls;
    },
  ) => Promise<Extraction>;
};

export type Extraction = {
  artwork?: IPicture[];
  stats?: TrackStats;
  tags?: TrackTags;
};

export type Urls = { get: string; head: string };
