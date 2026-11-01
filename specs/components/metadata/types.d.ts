import type { DiffuseElement } from "~/common/element.js";
import type { ProxiedActions } from "~/common/worker.d.ts";
import type { Track } from "~/definitions/types.d.ts";

export type Actions = {
  patch(track: Track): Promise<Track>;

  /**
   * Best-effort cancellation of an in-flight `patch` for a track (e.g. when
   * the track stops playing). Providers that can't cancel simply omit it.
   */
  abort?(trackId: string): void;
};

export type MetadataElement = DiffuseElement & ProxiedActions<Actions>;
