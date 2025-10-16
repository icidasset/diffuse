import type { Track, TrackStats, TrackTags } from "@component/core/types.d.ts";
import type { Signal } from "@common/signal.d.ts";

export interface Actions {
  add: (items: Item[]) => void;
  // TODO
}

export type Item<Stats = TrackStats, Tags = TrackTags> =
  & Track<Stats, Tags>
  & { manualEntry?: boolean };

export interface Signals {
  future: Signal<Item[]>;
  now: Signal<Item | null>;
  past: Signal<Item[]>;
}
