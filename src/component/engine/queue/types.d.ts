import type { Track, TrackStats, TrackTags } from "@component/core/types.d.ts";
import type { Signal } from "@common/signal.d.ts";

export type Actions = {
  add: (items: Item[]) => void;
  pool: (tracks: Track[]) => void;
  shift: () => void;
  unshift: () => void;
};

export type Item<Stats = TrackStats, Tags = TrackTags> =
  & Track<Stats, Tags>
  & { manualEntry?: boolean };

export type Signals = {
  future: Signal<Item[]>;
  now: Signal<Item | null>;
  past: Signal<Item[]>;
};
