/* INPUT */

/**
 * Consultation.
 *
 * `consult` can be "undetermined" if only a scheme was given
 * instead of a full URI.
 */
export type Consult =
  | { supported: false; reason: string }
  | { supported: true; consult: "undetermined" | boolean };

export type ConsultGrouping =
  | { available: false; reason: string; tracks: Track[] }
  | { available: true; tracks: Track[] };

export type GroupConsult = Record<string, ConsultGrouping>;

export type InputActions = {
  consult(fileUriOrScheme: string): Promise<Consult>;
  contextualize(tracks: Track[]): Promise<void>;
  groupConsult(tracks: Track[]): Promise<GroupConsult>;
  list(cachedTracks: Track[]): Promise<Track[]>;
  resolve(
    { method, uri }: { method: string; uri: string },
  ): Promise<ResolvedUri>;
};

export type InputElement = HTMLElement & InputActions;

/* TRACKS */

export type ResolvedUri = undefined | { url: string; expiresAt: number }; // TODO: Streams?

export interface Track<Stats = TrackStats, Tags = TrackTags> {
  id: string;

  kind?: "music" | "audiobook" | "podcast" | "placeholder" | "miscellaneous";
  stats?: Stats;
  tags?: Tags;

  // NOTE: This is a "semi-permanent" URI.
  //
  // Tracks are cached so you can't, for example,
  // use an URL that expires in several hours.
  uri: string;
}

export interface TrackStats {
  /** Album gain in dB */
  albumGain?: number;

  /** Bits per second */
  bitrate?: number;

  /** Bit depth */
  bitsPerSample?: number;

  /** Compression algorithm used */
  codec?: string;

  /** Encoding format used */
  container?: string;

  /** Duration in seconds */
  duration?: number;

  /** Is track lossless? */
  lossless?: boolean;

  /** Number of audio channels */
  numberOfChannels?: number;

  /** Samples per second */
  sampleRate?: number;

  /** Track gain in dB */
  trackGain?: number;
}

export interface TrackTags {
  album?: string;
  albumartist?: string;
  albumartists?: string[];
  albumartistsort?: string;
  albumsort?: string;
  arranger?: string[];
  artist?: string;
  artists?: string[];
  artistsort?: string;
  asin?: string;
  averageLevel?: number;
  barcode?: string;
  bpm?: number;
  catalognumbers?: string[];
  compilation?: boolean;
  composers?: string[];
  composersort?: string;
  conductors?: string[];
  date?: string;
  disc: { no: number; of?: number };
  djmixers?: string[];
  engineers?: string[];
  gapless?: boolean;
  genres?: string[];
  isrc?: string[];
  labels?: string[];
  lyricists?: string[];
  media?: string;
  mixers?: string[];
  moods?: string[];
  originaldate?: string;
  originalyear?: number;
  peakLevel?: number;
  producers?: string[];
  publishers?: string[];
  releasecountry?: string;
  releasedate?: string;
  releasestatus?: string;
  releasetypes?: string[];
  remixers?: string[];
  technicians?: string[];
  title: string;
  titlesort?: string;
  track: { no: number; of?: number };
  work?: string;
  writers?: string[];
  year?: number;
}
