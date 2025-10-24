import "@component/input/opensubsonic/element.js";
import "@component/processor/metadata/element.js";

import * as Audio from "@component/engine/audio/element.js";
import * as Output from "@component/output/indexed-db/element.js";
import * as Queue from "@component/engine/queue/element.js";

import "@component/orchestrator/process-tracks/element.js";

import { component } from "@common/element.js";
import { effect } from "@common/signal.js";

/**
 * @import {Item} from "@component/engine/queue/types.d.ts"
 */

const audio = component(Audio);
const output = component(Output);
const queue = component(Queue);

globalThis.output = output;

// QUEUE

effect(() => {
  const now = queue.now();
  if (now === null) return;

  audio.supply({
    audio: [
      {
        id: now.id,
        isPreload: false,
        url: now.uri,
      },
    ],
  });
});
