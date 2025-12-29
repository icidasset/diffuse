import type { Orama, SearchParams } from "@orama/orama";

import type { SignalReader } from "@common/signal.d.ts";
import type { Track } from "@definitions/types.d.ts";
import type { SCHEMA } from "./constants.js";

export type Actions = {
  /**
   * https://docs.orama.com/docs/orama-js/search
   */
  search(params: SearchParams<Schema>): Promise<Track[]>;
  supply(args: { tracks: Track[] }): Promise<void>;
};

export type Schema = Orama<typeof SCHEMA>;

export type State = {
  cacheId: SignalReader<string>;
};
