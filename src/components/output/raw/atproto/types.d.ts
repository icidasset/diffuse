import type { SignalReader } from "@common/signal.d.ts";
import type { Track } from "@definitions/types.d.ts";
import type { OutputElement } from "../../types.d.ts";

export type ATProtoOutputElement =
  & OutputElement
  & {
    did: SignalReader<string | null>;
    rev: SignalReader<string | null>;

    /** Track records with encrypted URIs that cannot be decrypted without the passkey. */
    lockedTracks: SignalReader<Track[]>;

    /** True if passkey encryption is active for this session. */
    passkeyActive: SignalReader<boolean>;

    getLatestCommit(): Promise<string | null>;
    login(handle: string): Promise<void>;
    logout(): Promise<void>;

    /** Adopt an existing passkey from another device via discoverable-credential lookup. */
    adoptPasskey(): Promise<void>;

    /** Remove the stored passkey credential and clear the in-memory key. */
    removePasskey(): Promise<void>;

    /** Register a new passkey for track URI encryption. Throws if PRF is not supported. */
    setupPasskey(): Promise<void>;
  };
