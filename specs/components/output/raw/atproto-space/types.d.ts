import type { SignalReader } from "~/common/signal.d.ts";
import type { OutputElement } from "@specs/components/output/types.d.ts";

export type ATProtoSpaceOutputElement =
  & OutputElement
  & {
    did: SignalReader<string | null>;
    handle: SignalReader<string | null>;

    login(handle: string): Promise<void>;
    logout(): Promise<void>;
    whenRestored(): Promise<void>;
  };
