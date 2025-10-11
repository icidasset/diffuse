import { xxh32r } from "xxh32/dist/raw.js";

import type { ManagedOutput, Track } from "@applets/core/types";
import type { DiffuseApplet } from "@scripts/applet/common";
import { jsonEncode } from "@scripts/common";

export const INITIAL_MANAGED_OUTPUT: ManagedOutput = {
  tracks: {
    cacheId: tracksCacheId([]),
    state: "loading",
    collection: [],
  },
};

export function outputManager<DataType>(args: {
  context: DiffuseApplet<DataType>;
  /* Indicate if the initial data loader may proceed. */
  init?: () => Promise<boolean>;
  tracks: {
    get(): Promise<Track[]>;
    put(tracks: Track[]): Promise<void>;
  };
}) {
  const { context } = args;

  // Initial data loader
  async function load() {
    await context.settled();

    if (!context.isMainInstance()) return;
    if (args.init && (await args.init()) === false) return;

    const collection = await tracks();

    context.data = {
      ...context.data,
      tracks: {
        cacheId: tracksCacheId(collection),
        state: "loaded",
        collection,
      },
    };
  }

  load();

  async function tracks(): Promise<Track[]>;
  async function tracks(tracks: Track[]): Promise<void>;
  async function tracks(tracks?: Track[]): Promise<Track[] | void> {
    if (tracks) {
      // PUT
      context.data = {
        ...context.data,
        tracks: {
          cacheId: tracksCacheId(tracks),
          state: "loaded",
          collection: tracks,
        },
      };

      await args.tracks.put(tracks);
    } else {
      // GET
      return await args.tracks.get();
    }
  }

  return {
    load,
    tracks,
  };
}

export function tracksCacheId(tracks: Track[]): string {
  // TODO: Probably should work with encoded tracks directly?
  return xxh32r(jsonEncode(tracks)).toString();
}
