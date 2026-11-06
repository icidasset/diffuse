import type { SignalReader } from "~/common/signal.d.ts";
import type { OutputElement } from "@specs/components/output/types.d.ts";

export type WnfsFirkinOutputElement =
  & OutputElement
  & {
    did: SignalReader<string | undefined>;
    apiBase: SignalReader<string | undefined>;
    dataRoot: SignalReader<string | undefined>;
    sessionExpiresAt: SignalReader<number | undefined>;

    /**
     * The DID the most recent passkey flow derived, even when that flow then
     * failed (e.g. the account is not registered with the instance yet).
     */
    lastPasskeyDid: SignalReader<string | undefined>;

    /**
     * Authenticate with a passkey, delegate all `/firkin` capabilities to a
     * browser session key, and load or create the WNFS.
     */
    signIn(options?: {
      apiBase?: string;
      create?: boolean;
    }): Promise<{ did: string; root: string; created: boolean }>;

    /**
     * Create a passkey (or authenticate with an existing one) and return the
     * DID it derives, without touching the session or the WNFS. Local only:
     * no request is made.
     */
    passkeyDid(options?: { create?: boolean }): Promise<string>;

    /** Forget the local session and storage pointer. */
    signOut(): Promise<void>;

    /** Resolves after the initial restore attempt. */
    whenRestored(): Promise<void>;
  };
