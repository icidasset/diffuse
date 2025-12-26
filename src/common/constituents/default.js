import Queue from "@components/engine/queue/element.js";
import InputOrchestrator from "@components/orchestrator/input/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import QueueTracksOrchestrator from "@components/orchestrator/queue-tracks/element.js";
import { effect } from "../signal.js";

export const GROUP = "constituents";

/**
 * Default config for constituents.
 */
export function config() {
  // Queue
  const queue = new Queue();
  queue.setAttribute("group", GROUP);

  document.body.append(queue);

  // Input
  const input = new InputOrchestrator();
  input.setAttribute("id", "input");

  document.body.append(input);

  // Output
  const output = new OutputOrchestrator();
  output.setAttribute("id", "output");

  document.body.append(output);

  // Orchestrators
  const oqt = new QueueTracksOrchestrator();
  oqt.setAttribute("group", GROUP);
  oqt.setAttribute("input-selector", "#input");
  oqt.setAttribute("output-selector", "#output");
  oqt.setAttribute("queue-engine-selector", queue.localName);

  document.body.append(oqt);

  // Signals & effects
  effect(() => {
    const trigger = queue.now();
    const _other_trigger = queue.poolHash();

    oqt.isLeader().then((isLeader) => {
      if (!isLeader) return;
      queue.fill({ amount: 10, shuffled: true });
      if (!trigger) queue.shift();
    });
  });

  // Return elements
  return {
    GROUP,

    configurator: {
      input,
      output,
    },
    engine: {
      queue,
    },
    orchestrator: {
      input,
      output,
      queueTracks: oqt,
    },
  };
}
