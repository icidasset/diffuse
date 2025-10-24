import "@component/input/opensubsonic/element.js";
import "@component/output/indexed-db/element.js";
import "@component/processor/metadata/element.js";

import * as Audio from "@component/engine/audio/element.js";
import * as Queue from "@component/engine/queue/element.js";

import "@component/orchestrator/process-tracks/element.js";

import { component } from "@common/element.js";
import { effect } from "@common/signal.js";

/**
 * @import {Item} from "@component/engine/queue/types.d.ts"
 */

const audio = component(Audio);
const queue = component(Queue);

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

queue.pool([
  {
    id: "test",
    uri:
      "https://archive.org/download/deathofsalesmans00mill/01_Side_1_Death_of_a_salesman_-_Introduction_Act_1__Part_1.mp3",
  },
]);
