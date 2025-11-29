import "@components/input/opensubsonic/element.js";
import "@components/processor/metadata/element.js";
import "@components/transformer/output/string/json/element.js";
import "@components/transformer/output/refiner/default/element.js";

import * as Audio from "@components/engine/audio/element.js";
import * as Output from "@components/output/polymorphic/indexed-db/element.js";
import * as Queue from "@components/engine/queue/element.js";

import { component } from "@common/element.js";
import { effect } from "@common/signal.js";

const audio = component(Audio);
const output = component(Output);
const queue = component(Queue);

globalThis.audio = audio;
globalThis.output = output;
globalThis.queue = queue;

// 🚀

isLeader().then((bool) => {
  if (!bool) return;

  // Only load orchestrators if leader
  import("@components/orchestrator/process-tracks/element.js");
  import("@components/orchestrator/queue-audio/element.js");
  import("@components/orchestrator/queue-tracks/element.js");
});

// EFFECTS

effect(() => {
  console.log("Active queue item:", queue.now());
});

effect(() => {
  console.log("Queue pool hash:", queue.poolHash());
});

/**
 * Make sure there's always some random tracks in the queue.
 */
effect(() => {
  const trigger = queue.now();
  const _other_trigger = queue.poolHash();

  isLeader().then((bool) => {
    if (bool) {
      queue.fill({ amount: 10, shuffled: true });
      if (!trigger) queue.shift();
    }
  });
});

// 🛠️

async function isLeader() {
  return await audio.isLeader();
}
