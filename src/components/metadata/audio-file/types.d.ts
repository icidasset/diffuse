import type { IPicture } from "music-metadata";
import type { TrackStats, TrackTags } from "~/definitions/types.d.ts";

export type Extraction = {
  artwork?: IPicture[];
  stats?: TrackStats;
  tags?: TrackTags;
};

export type Urls = { get: string; head: string };
