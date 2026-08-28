import type { ProxiedActions } from "~/common/worker.d.ts";

import type { Track } from "~/definitions/types.d.ts";
import type { Consult } from "@specs/components/input/types.d.ts";
import type { DiffuseElement } from "~/common/element.js";

export type UploadActions = {
  /**
   * Check if this uploader or an individual URI can be used.
   */
  consult(uriOrScheme: string): Promise<Consult>;

  /**
   * Builds a placeholder track for the scheme's account, used to "add" the
   * corresponding input component so it lists the uploaded files.
   *
   * `scheme` is used by the configurator to route to the correct upload
   * component; individual components ignore it (they know their own scheme).
   */
  createSource(args: {
    scheme: string;
    refreshToken: string;
    directoryPath: string;
  }): Promise<Track>;

  /**
   * Delete an uploaded track.
   */
  delete(uri: string): Promise<void>;

  /**
   * Upload a track.
   */
  upload(args: { file: File; uri: string; path?: string }): Promise<string>;
};

export type UploadElement =
  & DiffuseElement
  & UploadSchemeProvider
  & ProxiedActions<UploadActions>
  & {
    /**
     * Triggers the OAuth flow for this upload component, if applicable.
     * Not all upload components require OAuth.
     */
    authorize?(): void;
  };

export type UploadSchemeProvider = { SCHEME: string };
