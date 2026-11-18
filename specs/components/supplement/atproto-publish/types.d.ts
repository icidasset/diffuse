import type { SignalReader } from "~/common/signal.d.ts";
import type { DiffuseElement } from "~/common/element.js";

export type ATProtoPublishElement = DiffuseElement & {
  did: SignalReader<string | null>;
  error: SignalReader<string | null>;
  handle: SignalReader<string | null>;
  lastSyncedAt: SignalReader<number | null>;
  ready: SignalReader<boolean>;
  syncing: SignalReader<boolean>;

  /**
   * Publish the selected output's playlist items, facets and collaborations
   * to the account now. Resolves without doing anything while unauthenticated
   * or offline.
   */
  publish(): Promise<void>;

  /** Initiate the shared AT Protocol OAuth flow. */
  login(handle: string): Promise<void>;

  /** Sign out and revoke the shared AT Protocol session. */
  logout(): Promise<void>;

  /** Resolves once the initial session restore has settled. */
  whenRestored(): Promise<void>;
};
