import { Signal } from "@common/signals.d.ts";
import { Track } from "@elements/core/types.d.ts";

export type State = { tracks: Signal<Track[]> };
