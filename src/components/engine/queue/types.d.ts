import type { Track } from "@common/types.d.ts";
import type { SignalReader } from "@common/signal.d.ts";

export type Actions = {
  add: (args: { inFront?: boolean; tracks: Track[] }) => void;
  pool: (tracks: Track[]) => void;
  shift: () => void;
  unshift: () => void;
};

export type Item =
  & Track
  & { manualEntry?: boolean };

export type State = {
  future: SignalReader<Item[]>;
  now: SignalReader<Item | null>;
  past: SignalReader<Item[]>;
};
