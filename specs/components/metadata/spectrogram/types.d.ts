import type { TrackStats } from "~/definitions/types.d.ts";

/**
 * The spectral descriptors derived from a track's spectrogram. These are the
 * spectrogram-analysis fields stored on `TrackStats`.
 */
export type SpectralStats = {
  spectralCentroid: number;
  spectralRolloff: number;
  spectralSpread: number;
  spectralFlatness: number;
  spectralFlux: number;
};

/**
 * The spectrogram component re-uses the standard metadata `patch` action,
 * merging its spectral stats into the track's existing `stats`.
 */
export type SpectralTrackStats = Pick<
  TrackStats,
  | "spectralCentroid"
  | "spectralRolloff"
  | "spectralSpread"
  | "spectralFlatness"
  | "spectralFlux"
>;
