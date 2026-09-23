import { ostiary, rpc, workerProxy } from "~/common/worker.js";
import { removeUndefinedValuesFromRecord } from "~/common/utils.js";
import { analyseSpectrogram } from "./fft.js";

/**
 * @import {Track, TrackStats} from "~/definitions/types.d.ts"
 * @import {ActionsWithTunnel, ProxiedActions} from "~/common/worker.d.ts"
 * @import {InputActions} from "@specs/components/input/types.d.ts"
 * @import {Actions} from "@specs/components/metadata/types.d.ts"
 */

// Spectrogram analysis is a frequency-domain operation, so the analysis sample
// rate doesn't need to exceed the Nyquist frequency of the highest band we care
// about. 22.05 kHz covers the full audible range and keeps FFT frames cheap.
const ANALYSIS_SAMPLE_RATE = 22050;

// Skip analysis for tracks longer than this (milliseconds). Spectrogram
// analysis decodes the *entire* file, so very long tracks are prohibitively
// expensive. The existing audio-file metadata already captured their other
// stats; we just skip the spectral descriptors.
const MAX_DURATION_MS = 4 * 60 * 60 * 1000; // 4 hours

/**
 * Time budget for downloading the audio data to analyse. Without it a request
 * to a server that accepts the connection but never answers — a half-open
 * connection after a network change or laptop sleep — stays pending forever
 * while holding one of the browser's few connections to that origin.
 */
const FETCH_TIMEOUT_MS = 60_000;

const SPECTRAL_KEYS = [
  "spectralCentroid",
  "spectralRolloff",
  "spectralSpread",
  "spectralFlatness",
  "spectralFlux",
];

/**
 * @param {TrackStats | undefined} stats
 * @returns {boolean}
 */
function hasSpectralStats(stats) {
  if (!stats) return false;
  return SPECTRAL_KEYS.every((k) => stats[/** @type {keyof TrackStats} */ (k)] != null);
}

/**
 * Reads a `ReadableStream` into a single `ArrayBuffer`.
 *
 * @param {ReadableStream<Uint8Array>} stream
 * @param {AbortSignal} [signal] When aborted, pending reads reject (for
 *   fetch-backed streams the underlying request is cancelled too).
 * @returns {Promise<ArrayBuffer>}
 */
async function readStreamToArrayBuffer(stream, signal) {
  const reader = stream.getReader();

  const onAbort = () => reader.cancel().catch(() => {});
  signal?.addEventListener("abort", onAbort, { once: true });

  /** @type {Uint8Array[]} */
  const chunks = [];
  let total = 0;

  try {
    for (;;) {
      const { done, value } = await reader.read();
      if (done) break;
      if (value) {
        chunks.push(value);
        total += value.byteLength;
      }
    }
  } finally {
    signal?.removeEventListener("abort", onAbort);
  }

  // `reader.cancel()` resolves pending reads with `done: true`, so an abort
  // looks like a clean end-of-stream. Throw explicitly so truncated data is
  // never treated as a complete file.
  if (signal?.aborted) throw new Error("stream read aborted");

  const result = new Uint8Array(total);
  let offset = 0;
  for (const chunk of chunks) {
    result.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return result.buffer;
}

/**
 * Decodes an encoded audio `ArrayBuffer` into mono PCM samples at a fixed
 * analysis sample rate, using an `OfflineAudioContext`.
 *
 * Returns `null` when decoding is unavailable (e.g. older browsers that don't
 * expose `OfflineAudioContext` inside a worker) so the caller can gracefully
 * skip spectral analysis without losing the rest of the track's stats.
 *
 * @param {ArrayBuffer} arrayBuffer
 * @returns {Promise<{ samples: Float32Array; sampleRate: number } | null>}
 */
async function decodeToMono(arrayBuffer) {
  /** @type {typeof OfflineAudioContext | undefined} */
  const Ctx = globalThis.OfflineAudioContext ??
    /** @type {any} */ (globalThis).webkitOfflineAudioContext;
  if (!Ctx) return null;

  // A throwaway context is enough — `decodeAudioData` resamples to the
  // context's sample rate, giving us a consistent rate to analyse at.
  /** @type {OfflineAudioContext} */
  const ctx = new Ctx(1, 1, ANALYSIS_SAMPLE_RATE);

  let audioBuffer;
  try {
    audioBuffer = await ctx.decodeAudioData(arrayBuffer.slice(0));
  } catch (err) {
    console.warn("spectrogram: failed to decode audio", err);
    return null;
  }

  const channels = audioBuffer.numberOfChannels;
  if (channels === 0) return null;

  // Mix down to mono by averaging all channels.
  const length = audioBuffer.length;
  const mono = new Float32Array(length);
  for (let c = 0; c < channels; c++) {
    const data = audioBuffer.getChannelData(c);
    for (let i = 0; i < length; i++) mono[i] += data[i] / channels;
  }

  return { samples: mono, sampleRate: audioBuffer.sampleRate };
}

/**
 * @type {ActionsWithTunnel<Actions>['patch']}
 */
export async function patch({ data: track, ports }) {
  // Skip if we already analysed this track (caching).
  if (hasSpectralStats(track.stats)) return track;

  // Skip streams and very long tracks — decoding them is too costly.
  if (track.kind === "stream") return track;
  if (track.stats?.duration && track.stats.duration > MAX_DURATION_MS) {
    return track;
  }

  /** @type {ProxiedActions<InputActions>} */
  const input = workerProxy(() => {
    ports.input.start();
    return ports.input;
  });

  const resGet = await input.resolve({ method: "GET", uri: track.uri });
  if (!resGet) return track;

  // Turn whatever `resolve` gave us into an ArrayBuffer.
  const controller = new AbortController();
  const timeoutId = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);

  try {
    let arrayBuffer;
    if ("stream" in resGet) {
      // Aborting the fetch signal errors fetch-backed streams, which makes
      // the pending `read()` reject instead of waiting forever.
      arrayBuffer = await readStreamToArrayBuffer(
        /** @type {ReadableStream<Uint8Array>} */ (resGet.stream),
        controller.signal,
      );
    } else if ("url" in resGet) {
      const res = await fetch(resGet.url, { signal: controller.signal });
      if (!res.ok) return track;
      arrayBuffer = await res.arrayBuffer();
    } else {
      return track;
    }

    const decoded = await decodeToMono(arrayBuffer);
    if (!decoded) return track;

    const spectral = analyseSpectrogram(decoded.samples, decoded.sampleRate);

    /** @type {TrackStats} */
    const stats = removeUndefinedValuesFromRecord({
      ...track.stats,
      ...spectral,
    });

    return {
      ...track,
      stats,
      updatedAt: new Date().toISOString(),
    };
  } catch (err) {
    console.warn("spectrogram: failed to fetch audio data", err);
    return track;
  } finally {
    clearTimeout(timeoutId);
  }
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { patch });
});
