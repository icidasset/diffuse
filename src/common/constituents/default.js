import ArtworkProcessor from "@components/processor/artwork/element.js";
import AudioEngine from "@components/engine/audio/element.js";
import Queue from "@components/engine/queue/element.js";
import InputOrchestrator from "@components/orchestrator/input/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import MetadataProcessor from "@components/processor/metadata/element.js";
import ProcessTracksOrchestrator from "@components/orchestrator/process-tracks/element.js";
import QueueAudioOrchestrator from "@components/orchestrator/queue-audio/element.js";
import QueueTracksOrchestrator from "@components/orchestrator/queue-tracks/element.js";
import RepeatShuffleOrchestrator from "@components/orchestrator/repeat-shuffle/element.js";
import SearchProcessor from "@components/processor/search/element.js";
import SearchTracksOrchestrator from "@components/orchestrator/search-tracks/element.js";
import SourcesOrchestrator from "@components/orchestrator/sources/element.js";

/**
 * @import { DiffuseElement } from "@toko/diffuse/common/element.js";
 */

export const GROUP = "constituents";

/**
 * Default config for constituents.
 */
export function config() {
  // Input
  const input = new InputOrchestrator();
  input.setAttribute("group", GROUP);
  input.setAttribute("id", "input");

  document.body.append(input);

  // Output
  const output = new OutputOrchestrator();
  output.setAttribute("group", GROUP);
  output.setAttribute("id", "output");

  document.body.append(output);

  // Processors
  const metadata = new MetadataProcessor();
  metadata.setAttribute("group", GROUP);

  document.body.append(metadata);

  // Orchestrators
  const opt = new ProcessTracksOrchestrator();
  opt.setAttribute("group", GROUP);
  opt.setAttribute("input-selector", input.selector);
  opt.setAttribute("output-selector", output.selector);
  opt.setAttribute("metadata-processor-selector", metadata.selector);
  opt.toggleAttribute("process-when-ready");

  document.body.append(opt);

  // LAZY
  // ----

  // Engines
  function audio() {
    const a = new AudioEngine();
    a.setAttribute("group", GROUP);

    addToBodyIfNeeded(a);
    return a;
  }

  function queue() {
    const q = new Queue();
    q.setAttribute("group", GROUP);

    addToBodyIfNeeded(q);
    return q;
  }

  // Processors
  function artwork() {
    const a = new ArtworkProcessor();
    a.setAttribute("group", GROUP);

    addToBodyIfNeeded(a);
    return a;
  }

  function search() {
    const s = new SearchProcessor();
    s.setAttribute("group", GROUP);

    addToBodyIfNeeded(s);
    return s;
  }

  // Orchestrators
  function queueAudio() {
    const a = audio();
    const q = queue();

    const oqa = new QueueAudioOrchestrator();
    oqa.setAttribute("group", GROUP);
    oqa.setAttribute("audio-engine-selector", a.selector);
    oqa.setAttribute("input-selector", input.selector);
    oqa.setAttribute("queue-engine-selector", q.selector);

    addToBodyIfNeeded(oqa);
    return oqa;
  }

  function queueTracks() {
    const q = queue();

    const oqt = new QueueTracksOrchestrator();
    oqt.setAttribute("group", GROUP);
    oqt.setAttribute("input-selector", input.selector);
    oqt.setAttribute("output-selector", output.selector);
    oqt.setAttribute("queue-engine-selector", q.selector);

    addToBodyIfNeeded(oqt);
    return oqt;
  }

  function repeatShuffle() {
    const q = queue();

    const ors = new RepeatShuffleOrchestrator();
    ors.setAttribute("group", GROUP);
    ors.setAttribute("queue-engine-selector", q.selector);

    addToBodyIfNeeded(ors);
    return ors;
  }

  function searchTracks() {
    const s = search();

    const ost = new SearchTracksOrchestrator();
    ost.setAttribute("group", GROUP);
    ost.setAttribute("input-selector", input.selector);
    ost.setAttribute("output-selector", output.selector);
    ost.setAttribute("search-processor-selector", s.selector);

    addToBodyIfNeeded(ost);
    return ost;
  }

  function sources() {
    const so = new SourcesOrchestrator();
    so.setAttribute("group", GROUP);
    so.setAttribute("input-selector", input.selector);
    so.setAttribute("output-selector", output.selector);

    addToBodyIfNeeded(so);
    return so;
  }

  // Return elements
  return {
    GROUP,

    engine: {},
    orchestrator: {
      input,
      output,
      processTracks: opt,
    },
    processor: {
      metadata,
    },

    lazy: {
      engine: {
        audio,
        queue,
      },
      orchestrator: {
        queueAudio,
        queueTracks,
        repeatShuffle,
        searchTracks,
        sources,
      },
      processor: {
        artwork,
        search,
      },
    },
  };
}

/**
 * @param {DiffuseElement} element
 */
export function addToBodyIfNeeded(element) {
  const alreadyAdded = document.body.querySelector(element.selector);
  if (!alreadyAdded) document.body.append(element);
}
