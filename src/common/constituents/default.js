import Queue from "@components/engine/queue/element.js";
import InputOrchestrator from "@components/orchestrator/input/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import QueueTracksOrchestrator from "@components/orchestrator/queue-tracks/element.js";
import RepeatShuffleOrchestrator from "@components/orchestrator/repeat-shuffle/element.js";
import SearchProcessor from "@components/processor/search/element.js";
import SearchTracksOrchestrator from "@components/orchestrator/search-tracks/element.js";

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

  // Processors
  const search = new SearchProcessor();
  search.setAttribute("group", GROUP);

  document.body.append(search);

  // Orchestrators
  const oqt = new QueueTracksOrchestrator();
  oqt.setAttribute("group", GROUP);
  oqt.setAttribute("input-selector", "#input");
  oqt.setAttribute("output-selector", "#output");
  oqt.setAttribute("queue-engine-selector", queue.localName);

  const rso = new RepeatShuffleOrchestrator();
  rso.setAttribute("group", GROUP);
  rso.setAttribute("queue-engine-selector", queue.localName);

  const ost = new SearchTracksOrchestrator();
  ost.setAttribute("group", GROUP);
  ost.setAttribute("input-selector", "#input");
  ost.setAttribute("output-selector", "#output");
  ost.setAttribute("search-processor-selector", search.localName);

  document.body.append(oqt, rso, ost);

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
      repeatShuffle: rso,
    },
    processor: {
      search,
    },
  };
}
