import type { Track } from "~/definitions/types.d.ts";
import type { DiffuseElement } from "~/common/element.js";
import type { ProxiedActions } from "~/common/worker.d.ts";

export type Actions = {
  get(track: Track): Promise<Uint8Array | null>;
};

export type ArtworkElement = DiffuseElement & ProxiedActions<Actions>;
