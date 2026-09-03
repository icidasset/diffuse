import type { Signal, SignalReader } from "~/common/signal.d.ts";
import type { Track } from "~/definitions/types.d.ts";

export type Actions = {
  adjustVolume: (_: { audioId?: string; volume: number }) => void;
  pause: (_: { audioId: string }) => void;
  play: (_: { audioId: string; volume?: number }) => void;
  reload: (_: { audioId: string; play: boolean; progress?: number }) => void;
  seek: (
    _: { currentTime?: number; audioId: string; percentage?: number },
  ) => void;
  state: (audioId: string) => AudioStateReadOnly | undefined;
  supply: (
    _: { audio: Audio[]; play?: { audioId: string; volume?: number } },
  ) => void;
};

export type Audio = AudioUrl | AudioStream;

export type AudioUrl = AudioBase & { url: string };

export type AudioStream = AudioBase & {
  stream: ReadableStream;

  /** Total duration in seconds. */
  duration?: number;

  /** Return a new stream starting at the given time. Required for seeking on stream items. */
  seek?: (timeSeconds: number) => Promise<ReadableStream>;
};

export type AudioBase = {
  id: string;
  isPreload: boolean;
  mimeType?: string;
  /**
   * Initial progress
   */
  progress?: number;
  track: Track;
};

export type AudioState = {
  currentTime: Signal<number>;
  duration: Signal<number>;
  hasEnded: Signal<boolean>;
  isPlaying: Signal<boolean>;
  isPreload: Signal<boolean>;
  loadingState: Signal<LoadingState>;

  // Computed
  progress: SignalReader<number>;
};

export type AudioStateReadOnly = {
  id: string;
  url: string;
  mimeType: string | undefined;

  /** https://developer.mozilla.org/en-US/docs/Web/API/BaseAudioContext/currentTime */
  currentTime: SignalReader<number>;
  duration: SignalReader<number>;
  hasEnded: SignalReader<boolean>;
  isPlaying: SignalReader<boolean>;
  isPreload: SignalReader<boolean>;
  loadingState: SignalReader<LoadingState>;

  // Computed
  progress: SignalReader<number>;
};

/**
 * The shared Web Audio graph that every <audio> element is routed through.
 *
 *   source → input → destination
 *
 * `input` is the post-volume tap point that all `<audio>` sources feed into;
 * it's connected straight to `destination` by default. A consumer applies
 * processing (an equalizer, an analyser for visualizations, …) by disconnecting
 * the `input`→`destination` edge and reconnecting it through its own chain.
 */
export type WebAudioGraph = {
  context: AudioContext;
  /** Post-volume tap point that every <audio> element feeds into (`gain` = master volume). */
  input: GainNode;
  destination: AudioDestinationNode;
};

export type LoadingState =
  | "initialisation"
  | "loading"
  | "loaded"
  | {
    error: { code: number };
  };

export type State = {
  isPlaying: SignalReader<boolean>;
  items: SignalReader<Audio[]>;
  volume: SignalReader<number>;
};
