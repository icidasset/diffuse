import type { OutputElement } from "@components/output/types.d.ts";
import type { SignalReader } from "@common/signal.d.ts";

export type OutputFallbackConfiguratorElement<Encoding = null> =
  & OutputElement<Encoding | undefined>
  & {
    activeOutput: SignalReader<OutputElement<Encoding> | null>;
  };
