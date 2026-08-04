import type { SignalReader } from "~/common/signal.d.ts";

export type Actions = {
  add: (args: { inFront?: boolean; trackIds: string[] }) => void;
  /**
   * Clear the `future()` items.
   */
  clear: (args: { keepManual?: boolean }) => void;
  expel: (args: { key: string }) => void;
  fill: (
    args: {
      /** Always keep adding, even if the amount of non-manual items in the queue are passed the given `amount` */
      augment?: boolean;
      amount: number;
      shuffled: boolean;
    },
  ) => void;
  move: (args: { key: string; to: number }) => void;
  shift: (args?: { by?: number }) => void;
  supply: (args: { trackIds: string[] }) => void;
  unshift: (args?: { by?: number }) => void;
};

export type Item = {
  id: string;
  key: string;
  manualEntry: boolean;
};

export type State = {
  future: SignalReader<Item[]>;
  now: SignalReader<Item | null>;
  past: SignalReader<Item[]>;

  /**
   * Initially this is set to `undefined`, but whenever the cache is changed afterwards this will be the hash of the items in the supply.
   */
  supplyFingerprint: SignalReader<string | undefined>;
};
