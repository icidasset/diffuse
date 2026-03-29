import type { DiffuseElement } from "~/common/element.js";
import type { ProxiedActions } from "~/common/worker.d.ts";
import type { Track } from "~/definitions/types.d.ts";

export type Actions = {
  patch(track: Track): Promise<Track>;
};

export type MetadataElement = DiffuseElement & ProxiedActions<Actions>;
