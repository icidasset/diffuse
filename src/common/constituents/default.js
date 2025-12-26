import InputConfigurator from "@components/configurator/input/element.js";
import Queue from "@components/engine/queue/element.js";
import OpenSubsonic from "@components/input/opensubsonic/element.js";
import S3 from "@components/input/s3/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import QueueTracksOrchestrator from "@components/orchestrator/queue-tracks/element.js";
import { effect } from "../signal.js";

export const GROUP = "constituents";

/**
 * Default config for constituents.
 */
export function config() {
  // Input
  const openSubsonic = new OpenSubsonic();
  const s3 = new S3();

  const input = new InputConfigurator();
  input.setAttribute("id", "input");
  input.append(openSubsonic, s3);

  document.body.append(input);

  // Queue
  const queue = new Queue();
  queue.setAttribute("group", GROUP);

  document.body.append(queue);

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
    input: {
      openSubsonic,
      s3,
    },
    orchestrator: {
      output,
      queueTracks: oqt,
    },
  };
}
