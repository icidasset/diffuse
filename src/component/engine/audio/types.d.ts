import type { Signal } from "@common/signal.d.ts";

export type Actions = {
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
  progress?: number;
  url: string;
};

export type AudioState = {
  duration: number;
  id: string;
  hasEnded: boolean;
  loadingState:
    | "initialisation"
    | "loading"
    | "loaded"
    | {
      error: { code: number };
    };
  isPlaying: boolean;
  isPreload: boolean;
  mimeType?: string;
  progress: number;
  url: string;
};

export type Signals = {
  isPlaying: Signal<boolean>;
  volume: Signal<number>;
};

export type State = Signals & {
  items: Signal<Audio[]>;
};
