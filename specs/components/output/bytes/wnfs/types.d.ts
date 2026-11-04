import type { SignalReader } from "~/common/signal.d.ts";
import type { OutputElement } from "@specs/components/output/types.d.ts";

export type DiffuseStorageOutputElement =
  & OutputElement
  & {
    did: SignalReader<string | undefined>;
    apiBase: SignalReader<string | undefined>;
    dataRoot: SignalReader<string | undefined>;
    sessionExpiresAt: SignalReader<number | undefined>;

    /**
     * Authenticate with a passkey, delegate all `/firkin` capabilities to a
     * browser session key, and load or create the WNFS.
     */
    signIn(options?: {
      apiBase?: string;
      adminSeed?: string;
      create?: boolean;
    }): Promise<{ did: string; root: string; created: boolean }>;

    /**
     * Authenticate with an existing passkey and return the DID it derives,
     * without touching the session or the WNFS.
     */
    passkeyDid(): Promise<string>;

    /** Forget the local session and storage pointer. */
    signOut(): Promise<void>;

    /** Resolves after the initial restore attempt. */
    whenRestored(): Promise<void>;
  };
