import { Signal } from "@common/signal.d.ts";
import { Track } from "@elements/core/types.d.ts";

export type State = { tracks: Signal<Track[]> };
