import type { Signal, SignalReader } from "@common/signal.d.ts";

export type Actions = {
  adjustVolume: (_: { audioId?: string; volume: number }) => void;
  pause: (_: { audioId: string }) => void;
  play: (_: { audioId: string; volume?: number }) => void;
  reload: (_: { audioId: string; play: boolean; progress?: number }) => void;
  seek: (_: { audioId: string; percentage: number }) => void;
  supply: (
    _: { audio: Audio[]; play?: { audioId: string; volume?: number } },
  ) => void;
};

export type Audio = {
  id: string;
  isPreload: boolean;
  mimeType?: string;
  // NOTE: Initial progress
  progress?: number;
  url: string;
};

export type AudioState = {
  duration: Signal<number>;
  hasEnded: Signal<boolean>;
  isPlaying: Signal<boolean>;
  isPreload: Signal<boolean>;
  loadingState: Signal<LoadingState>;
  progress: Signal<number>;
};

export type AudioStateReadOnly = {
  id: string;
  url: string;
  mimeType: string | undefined;

  duration: SignalReader<number>;
  hasEnded: SignalReader<boolean>;
  isPlaying: SignalReader<boolean>;
  isPreload: SignalReader<boolean>;
  loadingState: SignalReader<LoadingState>;
  progress: SignalReader<number>;
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
