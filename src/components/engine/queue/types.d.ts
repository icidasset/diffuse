import type { Track, TrackStats, TrackTags } from "@components/core/types.d.ts";
import type { SignalReader } from "@common/signal.d.ts";

export type Actions = {
  add: (args: { inFront?: boolean; items: Item[] }) => void;
  pool: (tracks: Track[]) => void;
  shift: () => void;
  unshift: () => void;
};

export type ActionsProxied = {
  add: (args: { inFront?: boolean; items: Item[] }) => Promise<void>;
  pool: (tracks: Track[]) => Promise<void>;
  shift: () => Promise<void>;
  unshift: () => Promise<void>;
};

export type Item<Stats = TrackStats, Tags = TrackTags> =
  & Track<Stats, Tags>
  & { manualEntry?: boolean };

export type State = {
  future: SignalReader<Item[]>;
  now: SignalReader<Item | null>;
  past: SignalReader<Item[]>;
};
