import "@components/engine/audio/element.js";
import "@components/input/opensubsonic/element.js";
import "@components/orchestrator/process-tracks/element.js";
import "@components/orchestrator/queue-audio/element.js";
import "@components/orchestrator/queue-tracks/element.js";
import "@components/processor/metadata/element.js";

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
