import type { OutputElement } from "@components/output/types.d.ts";
import type { SignalReader } from "@common/signal.d.ts";

export type OutputConfiguratorElement = OutputElement & {
  deselect: () => Promise<void>;
  options: () => Promise<
    Array<{
      id: string;
      label: string;
      element: OutputElement;
    }>
  >;
  select: (id: string) => Promise<void>;
  selectedOutput: SignalReader<OutputElement | null>;
};
