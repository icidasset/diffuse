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
export const config = {
  GROUP,

  /* Some predefined activity groups */
  assemblage: {
    playAudioFromQueue,
    queueManagement,
    searchThroughCollection,
  },

  // Elements
  engine: {
    audio,
    queue,
  },
  orchestrator: {
    input,
    output,
    queueAudio,
    queueTracks,
    processTracks,
    repeatShuffle,
    searchTracks,
    sources,
  },
  processor: {
    artwork,
    metadata,
    search,
  },
};

export default config;

// 📦️

function playAudioFromQueue() {
  const base = queueManagement();

  return {
    ...base,
    engine: {
      ...base.engine,
      audio: audio(),
    },
    orchestrator: {
      ...base.orchestrator,
      queueAudio: queueAudio(),
    },
  };
}

function queueManagement() {
  return {
    engine: {
      queue: queue(),
    },
    orchestrator: {
      input: input(),
      output: output(),
      processTracks: processTracks(),
      queueTracks: queueTracks(),
      repeatShuffle: repeatShuffle(),
    },
    processor: {
      metadata: metadata(),
    },
  };
}

function searchThroughCollection() {
  return {
    orchestrator: {
      output: output(),
      searchTracks: searchTracks(),
    },
    processor: {
      search: search(),
    },
  };
}

// 🥡

// Engines
function audio() {
  const a = new AudioEngine();
  a.setAttribute("group", GROUP);

  return findExistingOrAdd(a);
}

function queue() {
  const q = new Queue();
  q.setAttribute("group", GROUP);

  return findExistingOrAdd(q);
}

// Processors
function artwork() {
  const a = new ArtworkProcessor();
  a.setAttribute("group", GROUP);

  return findExistingOrAdd(a);
}

function metadata() {
  const m = new MetadataProcessor();
  m.setAttribute("group", GROUP);

  return findExistingOrAdd(m);
}

function search() {
  const s = new SearchProcessor();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s);
}

// Orchestrators
function input() {
  const i = new InputOrchestrator();
  i.setAttribute("group", GROUP);
  i.setAttribute("id", "input");

  return findExistingOrAdd(i);
}

function output() {
  const o = new OutputOrchestrator();
  o.setAttribute("group", GROUP);
  o.setAttribute("id", "output");

  return findExistingOrAdd(o);
}

function processTracks() {
  const i = input();
  const o = output();
  const m = metadata();

  const opt = new ProcessTracksOrchestrator();
  opt.setAttribute("group", GROUP);
  opt.setAttribute("input-selector", i.selector);
  opt.setAttribute("output-selector", o.selector);
  opt.setAttribute("metadata-processor-selector", m.selector);
  opt.toggleAttribute("process-when-ready");

  return findExistingOrAdd(opt);
}

function queueAudio() {
  const a = audio();
  const i = input();
  const q = queue();

  const oqa = new QueueAudioOrchestrator();
  oqa.setAttribute("group", GROUP);
  oqa.setAttribute("audio-engine-selector", a.selector);
  oqa.setAttribute("input-selector", i.selector);
  oqa.setAttribute("queue-engine-selector", q.selector);

  return findExistingOrAdd(oqa);
}

function queueTracks() {
  const i = input();
  const o = output();
  const q = queue();

  const oqt = new QueueTracksOrchestrator();
  oqt.setAttribute("group", GROUP);
  oqt.setAttribute("input-selector", i.selector);
  oqt.setAttribute("output-selector", o.selector);
  oqt.setAttribute("queue-engine-selector", q.selector);

  return findExistingOrAdd(oqt);
}

function repeatShuffle() {
  const q = queue();

  const ors = new RepeatShuffleOrchestrator();
  ors.setAttribute("group", GROUP);
  ors.setAttribute("queue-engine-selector", q.selector);

  return findExistingOrAdd(ors);
}

function searchTracks() {
  const i = input();
  const o = output();
  const s = search();

  const ost = new SearchTracksOrchestrator();
  ost.setAttribute("group", GROUP);
  ost.setAttribute("input-selector", i.selector);
  ost.setAttribute("output-selector", o.selector);
  ost.setAttribute("search-processor-selector", s.selector);

  return findExistingOrAdd(ost);
}

function sources() {
  const i = input();
  const o = output();
  const so = new SourcesOrchestrator();
  so.setAttribute("group", GROUP);
  so.setAttribute("input-selector", i.selector);
  so.setAttribute("output-selector", o.selector);

  return findExistingOrAdd(so);
}

// 🛠️

/**
 * @template {DiffuseElement} T
 * @param {T} element
 * @returns {T}
 */
export function findExistingOrAdd(element) {
  /** @type {T | null} */
  const alreadyAdded = document.body.querySelector(element.selector);
  if (!alreadyAdded) {
    document.body.append(element);
    return element;
  }

  return alreadyAdded;
}
