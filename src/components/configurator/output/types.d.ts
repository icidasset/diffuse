import type { OutputElement } from "~/components/output/types.d.ts";
import type { SignalReader } from "~/common/signal.d.ts";

export type OutputConfiguratorElement<ElementType = OutputElement> =
  & OutputElement
  & {
    deselect: () => Promise<void>;
    loadSelected: () => Promise<void>;
    options: () => Promise<Array<OutputOption<ElementType>>>;
    select: (id: string) => Promise<void>;

    /** Output-element ids that have been selected earlier. */
    activated: SignalReader<Set<string>>;

    /** Selected output element. */
    selected: SignalReader<ElementType | null>;
  };

export type OutputOption<ElementType = OutputElement> = {
  id: string;
  label: string;
  element: ElementType;
};
