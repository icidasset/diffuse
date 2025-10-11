import type { IPicture } from "music-metadata";
import type { TrackStats, TrackTags } from "@applets/core/types";

export type Extraction = { artwork?: IPicture[]; stats?: TrackStats; tags?: TrackTags };
export type Urls = { get: string; head: string };
