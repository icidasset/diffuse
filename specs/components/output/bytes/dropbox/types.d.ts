import type { SignalReader } from "~/common/signal.d.ts";
import type { OutputElement } from "@specs/components/output/types.d.ts";

export type DropboxOutputElement =
  & OutputElement<Uint8Array | undefined>
  & {
    refreshToken: SignalReader<string | undefined>;

    getRefreshToken(): Promise<string | undefined>;
    setRefreshToken(token: string): Promise<void>;
    unsetRefreshToken(): Promise<void>;

    authorize(): Promise<void>;
  };

export type DropboxOutputWorkerActions = {
  get(args: {
    refreshToken: string;
    name: string;
  }): Promise<Uint8Array | undefined>;
  put(args: {
    refreshToken: string;
    data: Uint8Array;
    name: string;
  }): Promise<void>;
};
