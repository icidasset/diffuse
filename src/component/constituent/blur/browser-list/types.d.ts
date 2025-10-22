import type { SignalReader } from "@common/signal.d.ts";
import type { Track } from "@component/core/types.d.ts";

export type State = { tracks: SignalReader<Track[]> };
