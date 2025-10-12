import { Signal } from "@common/signal.d.ts";

export interface State {
  isPlaying: boolean;
  items: Signal<Audio[]>;
  volume: { default: number };
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
