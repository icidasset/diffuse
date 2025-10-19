import type { Signal } from "../../../../common/signal.d.ts";
import type { Track } from "../../../core/types.d.ts";

export type State = { tracks: Signal<Track[]> };
