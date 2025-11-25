import "@components/engine/audio/element.js";
import "@components/input/opensubsonic/element.js";
import "@components/orchestrator/process-tracks/element.js";
import "@components/orchestrator/queue-audio/element.js";
import "@components/orchestrator/queue-tracks/element.js";
import "@components/processor/metadata/element.js";
import "@components/transformer/output/string/json/element.js";
import "@components/transformer/output/refiner/default/element.js";

import * as Output from "@components/output/polymorphic/indexed-db/element.js";
import * as Queue from "@components/engine/queue/element.js";

import { component } from "@common/element.js";
import { effect } from "@common/signal.js";

const output = component(Output);
const queue = component(Queue);

globalThis.output = output;
globalThis.queue = queue;

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
  const _trigger = queue.now();
  queue.fill({ amount: 10, shuffled: true });
});

effect(() => {
  const _trigger = queue.poolHash();
  queue.fill({ amount: 10, shuffled: true });

  // Automatically insert track if there isn't any
  if (!queue.now) queue.shift();
});
