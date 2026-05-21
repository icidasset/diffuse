import type { SignalReader } from "~/common/signal.d.ts";
import type { OutputElement } from "@specs/components/output/types.d.ts";

export type ATProtoOutputElement =
  & OutputElement
  & {
    did: SignalReader<string | null>;
    firehoseRev: SignalReader<{ rev: string; collections: ReadonlySet<string> } | null>;
    handle: SignalReader<string | null>;
    rev: SignalReader<string | null>;

    getLatestCommit(): Promise<string | null>;
    login(handle: string): Promise<void>;
    logout(): Promise<void>;
    whenRestored(): Promise<void>;
  };
