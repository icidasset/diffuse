import InputConfigurator from "@components/configurator/input/element.js";
import OutputConfigurator from "@components/configurator/output/element.js";
import Queue from "@components/engine/queue/element.js";
import OpenSubsonic from "@components/input/opensubsonic/element.js";
import S3 from "@components/input/s3/element.js";
import QueueTracksOrchestrator from "@components/orchestrator/queue-tracks/element.js";
import IndexedDBOutput from "@components/output/polymorphic/indexed-db/element.js";
import DefaultRefiner from "@components/transformer/output/refiner/default/element.js";
import JsonStringOutput from "@components/transformer/output/string/json/element.js";
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
  const idb = new IndexedDBOutput();
  idb.setAttribute("id", "idb-json-output")
  idb.setAttribute("namespace", "json")

  const json = new JsonStringOutput();
  json.setAttribute("id", "idb-json")
  json.setAttribute("output-selector", "#idb-json-output");

  const output = new OutputConfigurator();
  output.setAttribute("default", "idb-json");
  output.append(json);

  const refiner = new DefaultRefiner();
  refiner.setAttribute("id", "output");
  refiner.setAttribute("output-selector", output.localName);

  document.body.append(idb, output, refiner);

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
      queueTracks: oqt,
    },
    output: {
      indexedDB: idb,
    },
    transformer: {
      jsonStringOutput: json,
      refiner: {
        default: refiner,
      },
    },
  };
}
