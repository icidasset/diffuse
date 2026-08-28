/**
 * Radix-2 iterative Cooley-Tukey FFT and the spectral descriptors used by the
 * spectrogram metadata component.
 *
 * Everything here is pure (no DOM / worker APIs) so it can be unit-tested with
 * documentation tests, matching the rest of the codebase.
 */

/**
 * @import { SpectralStats } from "@specs/components/metadata/spectrogram/types.d.ts"
 */

////////////////////////////////////////////
// FFT
////////////////////////////////////////////

/**
 * In-place radix-2 Cooley-Tukey FFT. Mutates the provided real/imag arrays.
 *
 * `re` and `im` must be the same length, and the length must be a power of two.
 * Pass a negative `inverse` scale for the inverse transform.
 *
 * @param {Float32Array} re
 * @param {Float32Array} im
 * @param {number} n
 * @param {number} direction `1` for forward, `-1` for inverse.
 *
 * @example A pure DC signal transforms to a single non-zero bin at index 0
 * ```js
 * import { fft } from "~/components/metadata/spectrogram/fft.js";
 *
 * const n = 8;
 * const re = new Float32Array(n);
 * const im = new Float32Array(n);
 * for (let i = 0; i < n; i++) re[i] = 1; // constant signal
 * fft(re, im, n, 1);
 *
 * // DC bin carries the average, all other bins are ~0
 * if (Math.abs(re[0] - 1) > 1e-6) throw new Error("DC bin should be 1");
 * for (let i = 1; i < n; i++) {
 *   if (Math.abs(re[i]) > 1e-6 || Math.abs(im[i]) > 1e-6) {
 *     throw new Error("non-DC bins should be ~0");
 *   }
 * }
 * ```
 *
 * @example A sinusoid at bin k produces peaks at bins k and n-k
 * ```js
 * import { fft } from "~/components/metadata/spectrogram/fft.js";
 *
 * const n = 16;
 * const k = 3;
 * const re = new Float32Array(n);
 * const im = new Float32Array(n);
 * for (let i = 0; i < n; i++) re[i] = Math.cos((2 * Math.PI * k * i) / n);
 * fft(re, im, n, 1);
 *
 * // magnitude peak at k and n-k
 * if (Math.hypot(re[k], im[k]) < 0.4) throw new Error("expected peak at bin " + k);
 * if (Math.hypot(re[n - k], im[n - k]) < 0.4) throw new Error("expected peak at bin " + (n - k));
 * ```
 */
export function fft(re, im, n, direction) {
  // Bit-reversal permutation
  for (let i = 1, j = 0; i < n; i++) {
    let bit = n >> 1;
    for (; j & bit; bit >>= 1) j ^= bit;
    j ^= bit;

    if (i < j) {
      const tr = re[i];
      re[i] = re[j];
      re[j] = tr;
      const ti = im[i];
      im[i] = im[j];
      im[j] = ti;
    }
  }

  // Butterflies
  for (let len = 2; len <= n; len <<= 1) {
    const half = len >> 1;
    const angle = (direction * 2 * Math.PI) / len;
    const wRe = Math.cos(angle);
    const wIm = Math.sin(angle);

    for (let i = 0; i < n; i += len) {
      let curRe = 1;
      let curIm = 0;

      for (let k = 0; k < half; k++) {
        const aRe = re[i + k];
        const aIm = im[i + k];
        const bRe = re[i + k + half];
        const bIm = im[i + k + half];
        const tRe = curRe * bRe - curIm * bIm;
        const tIm = curRe * bIm + curIm * bRe;

        re[i + k] = aRe + tRe;
        im[i + k] = aIm + tIm;
        re[i + k + half] = aRe - tRe;
        im[i + k + half] = aIm - tIm;

        const nextRe = curRe * wRe - curIm * wIm;
        curIm = curRe * wIm + curIm * wRe;
        curRe = nextRe;
      }
    }
  }

  // Forward transform: divide by n to keep magnitudes in a comparable range.
  if (direction === 1) {
    for (let i = 0; i < n; i++) {
      re[i] /= n;
      im[i] /= n;
    }
  }
}

////////////////////////////////////////////
// WINDOW
////////////////////////////////////////////

/**
 * A Hann window of length `n`. Used to reduce spectral leakage at frame edges.
 *
 * @param {number} n
 * @returns {Float32Array}
 *
 * @example Endpoint values are zero, the midpoint (odd length) is 1
 * ```js
 * import { hannWindow } from "~/components/metadata/spectrogram/fft.js";
 *
 * const w = hannWindow(9); // odd length -> sample lands on the peak
 * if (w[0] > 1e-6 || w[8] > 1e-6) throw new Error("Hann endpoints should be ~0");
 * if (Math.abs(w[4] - 1) > 1e-6) throw new Error("Hann midpoint should be 1");
 * ```
 */
export function hannWindow(n) {
  const w = new Float32Array(n);
  for (let i = 0; i < n; i++) {
    w[i] = 0.5 - 0.5 * Math.cos((2 * Math.PI * i) / (n - 1));
  }
  return w;
}

////////////////////////////////////////////
// SPECTRAL DESCRIPTORS
////////////////////////////////////////////

/**
 * Frequency in Hz of FFT bin `i` for a given sample rate and FFT size.
 *
 * @param {number} i
 * @param {number} sampleRate
 * @param {number} fftSize
 * @returns {number}
 */
function binFrequency(i, sampleRate, fftSize) {
  return (i * sampleRate) / fftSize;
}

/**
 * Spectral centroid: the magnitude-weighted mean frequency, a measure of
 * perceptual brightness.
 *
 * @param {Float32Array} magnitude
 * @param {number} sampleRate
 * @param {number} fftSize
 * @returns {number}
 *
 * @example A single-bin spectrum has its centroid at that bin's frequency
 * ```js
 * import { spectralCentroid } from "~/components/metadata/spectrogram/fft.js";
 *
 * const fftSize = 8;
 * const sampleRate = 8;
 * const mag = new Float32Array(fftSize / 2 + 1);
 * mag[2] = 1; // all energy at bin 2 -> 2 Hz
 * if (Math.abs(spectralCentroid(mag, sampleRate, fftSize) - 2) > 1e-6) {
 *   throw new Error("centroid should equal the single bin's frequency");
 * }
 * ```
 */
export function spectralCentroid(magnitude, sampleRate, fftSize) {
  let weighted = 0;
  let total = 0;

  for (let i = 0; i < magnitude.length; i++) {
    const m = magnitude[i];
    if (m <= 0) continue;
    weighted += binFrequency(i, sampleRate, fftSize) * m;
    total += m;
  }

  return total > 0 ? weighted / total : 0;
}

/**
 * Spectral spread: the standard deviation of the spectrum around its centroid,
 * describing how concentrated or dispersed the spectral energy is.
 *
 * @param {Float32Array} magnitude
 * @param {number} sampleRate
 * @param {number} fftSize
 * @returns {number}
 *
 * @example A single-bin spectrum has zero spread
 * ```js
 * import { spectralSpread } from "~/components/metadata/spectrogram/fft.js";
 *
 * const fftSize = 8;
 * const sampleRate = 8;
 * const mag = new Float32Array(fftSize / 2 + 1);
 * mag[2] = 1;
 * if (spectralSpread(mag, sampleRate, fftSize) > 1e-6) {
 *   throw new Error("a single-bin spectrum should have zero spread");
 * }
 * ```
 */
export function spectralSpread(magnitude, sampleRate, fftSize) {
  const centroid = spectralCentroid(magnitude, sampleRate, fftSize);

  let weighted = 0;
  let total = 0;

  for (let i = 0; i < magnitude.length; i++) {
    const m = magnitude[i];
    if (m <= 0) continue;
    const f = binFrequency(i, sampleRate, fftSize);
    weighted += m * (f - centroid) ** 2;
    total += m;
  }

  return total > 0 ? Math.sqrt(weighted / total) : 0;
}

/**
 * Spectral rolloff: the lowest frequency below which `threshold` (0-1) of the
 * total spectral energy is contained.
 *
 * @param {Float32Array} magnitude
 * @param {number} sampleRate
 * @param {number} fftSize
 * @param {number} threshold Fraction of cumulative energy, e.g. 0.85.
 * @returns {number}
 *
 * @example All energy in the first bin -> rolloff at bin 0's frequency
 * ```js
 * import { spectralRolloff } from "~/components/metadata/spectrogram/fft.js";
 *
 * const fftSize = 8;
 * const sampleRate = 8;
 * const mag = new Float32Array(fftSize / 2 + 1);
 * mag[0] = 1;
 * if (spectralRolloff(mag, sampleRate, fftSize, 0.85) > 1e-6) {
 *   throw new Error("rolloff should be 0 when all energy is at bin 0");
 * }
 * ```
 */
export function spectralRolloff(magnitude, sampleRate, fftSize, threshold) {
  let total = 0;
  for (let i = 0; i < magnitude.length; i++) total += magnitude[i];
  if (total <= 0) return 0;

  const target = total * threshold;
  let cumulative = 0;

  for (let i = 0; i < magnitude.length; i++) {
    cumulative += magnitude[i];
    if (cumulative >= target) {
      return binFrequency(i, sampleRate, fftSize);
    }
  }

  return binFrequency(magnitude.length - 1, sampleRate, fftSize);
}

/**
 * Spectral flatness: geometric mean / arithmetic mean of the (positive)
 * magnitudes. Ranges from 0 (completely tonal) to 1 (completely noisy).
 *
 * @param {Float32Array} magnitude
 * @returns {number}
 *
 * @example A uniform spectrum is maximally flat (1)
 * ```js
 * import { spectralFlatness } from "~/components/metadata/spectrogram/fft.js";
 *
 * const mag = new Float32Array(5).fill(2);
 * if (Math.abs(spectralFlatness(mag) - 1) > 1e-6) {
 *   throw new Error("a uniform spectrum should have flatness 1");
 * ```
 *
 * @example Concentrated (tonal) energy yields a flatness near 0
 * ```js
 * import { spectralFlatness } from "~/components/metadata/spectrogram/fft.js";
 *
 * const mag = new Float32Array([1000, 1, 1, 1, 1]); // one dominant bin
 * if (spectralFlatness(mag) > 0.1) {
 *   throw new Error("a tonal spectrum should have flatness near 0");
 * }
 * ```
 */
export function spectralFlatness(magnitude) {
  let count = 0;
  let logSum = 0;
  let sum = 0;

  for (let i = 0; i < magnitude.length; i++) {
    const m = magnitude[i];
    if (m <= 0) continue;
    count++;
    logSum += Math.log(m);
    sum += m;
  }

  if (count === 0 || sum === 0) return 0;
  const geometricMean = Math.exp(logSum / count);
  const arithmeticMean = sum / count;
  return arithmeticMean > 0 ? geometricMean / arithmeticMean : 0;
}

/**
 * Per-frame spectral flux: the sum of *positive* magnitude differences between
 * consecutive frames, measuring how much new spectral energy appears.
 *
 * @param {Float32Array} prev Previous frame magnitudes.
 * @param {Float32Array} curr Current frame magnitudes.
 * @returns {number}
 *
 * @example Identical frames produce zero flux
 * ```js
 * import { spectralFlux } from "~/components/metadata/spectrogram/fft.js";
 *
 * const m = new Float32Array(4).fill(1);
 * if (spectralFlux(m, m) > 1e-6) {
 *   throw new Error("identical frames should have zero flux");
 * }
 * ```
 *
 * @example Only positive differences count (decays are ignored)
 * ```js
 * import { spectralFlux } from "~/components/metadata/spectrogram/fft.js";
 *
 * const prev = new Float32Array([1, 1, 1, 1]);
 * const curr = new Float32Array([2, 0, 0, 0]); // +1 at bin 0, rest decrease
 * if (Math.abs(spectralFlux(prev, curr) - 1) > 1e-6) {
 *   throw new Error("only positive differences should be counted");
 * }
 * ```
 */
export function spectralFlux(prev, curr) {
  let sum = 0;
  const len = Math.min(prev.length, curr.length);
  for (let i = 0; i < len; i++) {
    const diff = curr[i] - prev[i];
    if (diff > 0) sum += diff * diff;
  }
  return sum;
}

////////////////////////////////////////////
// SPECTROGRAM ANALYSIS
////////////////////////////////////////////

/**
 * Computes a magnitude spectrogram from mono PCM samples via the STFT, then
 * reduces it to a compact set of spectral descriptors suitable for storing on
 * `TrackStats`.
 *
 * @param {Float32Array} samples Mono PCM samples.
 * @param {number} sampleRate Sample rate of `samples` (Hz).
 * @returns {SpectralStats}
 *
 * @example Silence yields all-zero descriptors
 * ```js
 * import { analyseSpectrogram } from "~/components/metadata/spectrogram/fft.js";
 *
 * const stats = analyseSpectrogram(new Float32Array(4096), 22050);
 * if (stats.spectralCentroid !== 0) throw new Error("silence: centroid should be 0");
 * if (stats.spectralFlatness !== 0) throw new Error("silence: flatness should be 0");
 * if (stats.spectralFlux !== 0) throw new Error("silence: flux should be 0");
 * ```
 */
export function analyseSpectrogram(samples, sampleRate) {
  const fftSize = 2048;
  const hopSize = 1024;
  const window = hannWindow(fftSize);
  const bins = fftSize / 2 + 1;

  const re = new Float32Array(fftSize);
  const im = new Float32Array(fftSize);

  /** @type {Float32Array[]} */
  const frames = [];
  /** @type {number[]} */
  const centroids = [];
  /** @type {number[]} */
  const spreads = [];
  /** @type {number[]} */
  const rolloffs = [];
  /** @type {number[]} */
  const flatnesses = [];
  /** @type {number[]} */
  const fluxes = [];

  /** @type {Float32Array | null} */
  let prevMag = null;

  for (let start = 0; start + fftSize <= samples.length; start += hopSize) {
    for (let i = 0; i < fftSize; i++) {
      re[i] = samples[start + i] * window[i];
      im[i] = 0;
    }

    fft(re, im, fftSize, 1);

    const mag = new Float32Array(bins);
    for (let i = 0; i < bins; i++) {
      mag[i] = Math.hypot(re[i], im[i]);
    }
    frames.push(mag);

    centroids.push(spectralCentroid(mag, sampleRate, fftSize));
    spreads.push(spectralSpread(mag, sampleRate, fftSize));
    rolloffs.push(spectralRolloff(mag, sampleRate, fftSize, 0.85));
    flatnesses.push(spectralFlatness(mag));

    if (prevMag) {
      fluxes.push(spectralFlux(prevMag, mag));
    }
    prevMag = mag;
  }

  if (frames.length === 0) {
    return {
      spectralCentroid: 0,
      spectralRolloff: 0,
      spectralSpread: 0,
      spectralFlatness: 0,
      spectralFlux: 0,
    };
  }

  const mean = (/** @type {number[]} */ xs) =>
    xs.reduce((a, b) => a + b, 0) / xs.length;

  return {
    spectralCentroid: Math.round(mean(centroids)),
    spectralRolloff: Math.round(mean(rolloffs)),
    spectralSpread: Math.round(mean(spreads)),
    // Flatness is 0-1; store as a 0-1000 integer (per-mille).
    spectralFlatness: Math.round(mean(flatnesses) * 1000),
    // Flux is a small float; scale by 1000 for integer storage.
    spectralFlux: Math.round(mean(fluxes.length ? fluxes : [0]) * 1000),
  };
}
