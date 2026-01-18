import Queue from "@components/engine/queue/element.js";
import InputOrchestrator from "@components/orchestrator/input/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import MetadataProcessor from "@components/processor/metadata/element.js";
import ProcessTracksOrchestrator from "@components/orchestrator/process-tracks/element.js";
import QueueTracksOrchestrator from "@components/orchestrator/queue-tracks/element.js";
import RepeatShuffleOrchestrator from "@components/orchestrator/repeat-shuffle/element.js";
import SearchProcessor from "@components/processor/search/element.js";
import SearchTracksOrchestrator from "@components/orchestrator/search-tracks/element.js";
import SourcesOrchestrator from "@components/orchestrator/sources/element.js";

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
  const metadata = new MetadataProcessor();
  metadata.setAttribute("group", GROUP);

  document.body.append(metadata);

  const search = new SearchProcessor();
  search.setAttribute("group", GROUP);

  document.body.append(search);

  // Orchestrators
  const opt = new ProcessTracksOrchestrator();
  opt.setAttribute("group", GROUP);
  opt.setAttribute("input-selector", input.selector);
  opt.setAttribute("output-selector", output.selector);
  opt.setAttribute("metadata-processor-selector", metadata.selector);
  opt.toggleAttribute("process-when-ready");

  const oqt = new QueueTracksOrchestrator();
  oqt.setAttribute("group", GROUP);
  oqt.setAttribute("input-selector", input.selector);
  oqt.setAttribute("output-selector", output.selector);
  oqt.setAttribute("queue-engine-selector", queue.selector);

  const ors = new RepeatShuffleOrchestrator();
  ors.setAttribute("group", GROUP);
  ors.setAttribute("queue-engine-selector", queue.selector);

  const ost = new SearchTracksOrchestrator();
  ost.setAttribute("group", GROUP);
  ost.setAttribute("input-selector", input.selector);
  ost.setAttribute("output-selector", output.selector);
  ost.setAttribute("search-processor-selector", search.selector);

  const osr = new SourcesOrchestrator();
  osr.setAttribute("group", GROUP);
  osr.setAttribute("input-selector", input.selector);
  osr.setAttribute("output-selector", output.selector);

  document.body.append(opt, oqt, ors, ost, osr);

  // Return elements
  return {
    GROUP,

    engine: {
      queue,
    },
    orchestrator: {
      input,
      output,
      processTracks: opt,
      queueTracks: oqt,
      repeatShuffle: ors,
      searchTracks: ost,
      sources: osr,
    },
    processor: {
      metadata,
      search,
    },
  };
}
