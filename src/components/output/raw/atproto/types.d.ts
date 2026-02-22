import type { SignalReader } from "@common/signal.d.ts";
import type { OutputElement } from "../../types.d.ts";

export type ATProtoOutputElement =
  & OutputElement
  & {
    did: SignalReader<string | null>;
    rev: SignalReader<string | null>;
    getLatestCommit(): Promise<string | null>;
    login(handle: string): Promise<void>;
    logout(): Promise<void>;
  };
