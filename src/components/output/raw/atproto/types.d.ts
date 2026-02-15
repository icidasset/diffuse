import type { Signal } from "@common/signal.d.ts";
import type { OutputElement } from "../../types.d.ts";

export type ATProtoOutputElement =
  & OutputElement
  & {
    $did: Signal<string | null>;
    login(handle: string): Promise<void>;
    logout(): Promise<void>;
  };
