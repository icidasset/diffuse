import type { Track, TrackStats, TrackTags } from "@component/core/types.d.ts";
import type { SignalReader } from "@common/signal.d.ts";

export type Actions = {
  add: (items: Item[]) => void;
  pool: (tracks: Track[]) => void;
  shift: () => void;
  unshift: () => void;
};

export type Item<Stats = TrackStats, Tags = TrackTags> =
  & Track<Stats, Tags>
  & { manualEntry?: boolean };

export type State = {
  future: SignalReader<Item[]>;
  now: SignalReader<Item | null>;
  past: SignalReader<Item[]>;
};
