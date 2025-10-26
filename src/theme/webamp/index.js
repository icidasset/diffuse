import Webamp from "webamp/lazy";
import { throttle } from "throttle-debounce";

// import "@component/orchestrator/process-tracks/element.js";
import "@component/orchestrator/queue-tracks/element.js";
import "@component/output/indexed-db/element.js";
import "@component/processor/metadata/element.js";

import * as Input from "@component/input/opensubsonic/element.js";
import * as Queue from "@component/engine/queue/element.js";

import { component } from "@common/element.js";
import { effect, signal, untracked } from "@common/signal.js";
import deepDiff from "@fry69/deep-diff";
import { debounceMicrotask } from "@vicary/debounce-microtask";

/**
 * @import {Diff} from "@fry69/deep-diff"
 * @import {URLTrack} from "webamp"
 *
 * @import {Track} from "@component/core/types.d.ts"
 * @import {Item} from "@component/engine/queue/types.d.ts"
 */

const input = component(Input);
const queue = component(Queue);

globalThis.queue = queue;

////////////////////////////////////////////
// ⚡
////////////////////////////////////////////

/** @type {import("webamp/lazy").default} */
const amp = new /** @type {any} */ (Webamp)({
  enableMediaSession: true,
  initialTracks: [],

  /** */
  handleLoadListEvent: async () => {
    // TODO
    return [
      /* Array of Tracks */
    ];
  },

  /**
   * @param {any} tracks
   */
  handleSaveListEvent: (tracks) => {
    // TODO
  },
});

// Override
const loadFromUrl = amp.media.loadFromUrl.bind(amp.media);

/**
 * @param {string} uri
 * @param {boolean} autoPlay
 */
async function loadOverride(uri, autoPlay) {
  const resp = await input.resolve({ method: "GET", uri });
  if (!resp) throw new Error("Failed to resolve URI");
  return await loadFromUrl(resp.url, autoPlay);
}

amp.media.loadFromUrl = loadOverride.bind(amp.media);

// Render
const ampNode = document.createElement("div");
ampNode.style =
  "height: 100vh; left: 0; position: absolute; top: 0; width: 100%; z-index: -1000;";
document.body.appendChild(ampNode);
amp.renderWhenReady(ampNode);

////////////////////////////////////////////
// 🌊
////////////////////////////////////////////

const $currTrack = signal(/** @type {null | number} */ (null));
const $playlist = signal(/** @type {Item[]} */ ([]));

/**
 * Observe changes in Webamp's internal store.
 */
amp.store.subscribe(() => {
  const state = amp.store.getState();
  $currTrack.value = state.playlist.currentTrack;
});

/**
 * Whenever the queue changes update the playlist.
 */
effect(() => {
  const now = queue.now();
  const past = queue.past();
  const future = queue.future();

  const playlist = [
    ...past,
    ...(now ? [now] : []),
    ...future,
  ];

  const diff = deepDiff.diff($playlist.value, playlist, () => true);

  diff?.forEach((d) => {
    // TODO: Handle case where an item is inserted into queue at a position that's not the end.
    // console.log(d);

    if (d.kind !== "A") return;
    if (d.item.kind === "N") {
      const item = /** @type {Item} */ (/** @type {unknown} */ (d.item.rhs));
      if (!item) return;

      /** @type {URLTrack} */
      const urlTrack = {
        url: item.uri,
        metaData: {
          title: item.tags?.title || "",
          artist: item.tags?.artist || "",
          album: item.tags?.album,
        },
        duration: item.stats?.duration,
      };

      amp.appendTracks([urlTrack]);
    }
  });

  if (!diff) return;

  $playlist.value = playlist;

  if (untracked($currTrack.get) === null) {
    amp.setCurrentTrack(past.length);
  }
});

/**
 * Whenever Webamp's queue changes,
 * reflect the change in our queue too.
 */
effect(() => {
  if (($currTrack.value ?? 0) > untracked(queue.past).length) {
    queue.shift();
  }
});
