import type { Signal } from "../../../common/signal.d.ts";

export interface Actions {
  pause: (_: { audioId: string }) => void;
  play: (_: { audioId: string; volume?: number }) => void;
  reload: (_: { audioId: string; play: boolean; progress?: number }) => void;
  seek: (_: { audioId: string; percentage: number }) => void;
  yield: (
    _: { audio: Audio[]; play?: { audioId: string; volume?: number } },
  ) => void;
}

export interface Audio {
  id: string;
  isPreload: boolean;
  mimeType?: string;
  progress?: number;
  url: string;
}

export interface AudioState {
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
}

export interface Signals {
  isPlaying: Signal<boolean>;
  items: Signal<Audio[]>;
  volume: Signal<number>;
}

export type State = Signals;
