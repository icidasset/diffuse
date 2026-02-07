import type { Track } from "@definitions/types.d.ts";
import type { SignalReader } from "@common/signal.d.ts";

export type Actions = {
  add: (args: { inFront?: boolean; tracks: Track[] }) => void;
  /**
   * Clear the `future()` items.
   */
  clear: (args: { manualOnly?: boolean }) => void;
  fill: (
    args: {
      /** Always keep adding, even if the amount of non-manual items in the queue are passed the given `amount` */
      augment?: boolean;
      amount: number;
      shuffled: boolean;
    },
  ) => void;
  shift: () => void;
  supply: (args: { tracks: Track[] }) => void;
  unshift: () => void;
};

export type Item =
  & Track
  & { manualEntry?: boolean };

export type State = {
  future: SignalReader<Item[]>;
  now: SignalReader<Item | null>;
  past: SignalReader<Item[]>;

  /**
   * Initially this is set to `undefined`, but whenever the cache is changed afterwards this will be the hash of the items in the supply.
   */
  supplyFingerprint: SignalReader<string | undefined>;
};
