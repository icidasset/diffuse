import ArtworkProcessor from "@components/processor/artwork/element.js";
import AudioEngine from "@components/engine/audio/element.js";
import AutoQueueOrchestrator from "@components/orchestrator/auto-queue/element.js";
import Queue from "@components/engine/queue/element.js";
import InputOrchestrator from "@components/orchestrator/input/element.js";
import OutputOrchestrator from "@components/orchestrator/output/element.js";
import MetadataProcessor from "@components/processor/metadata/element.js";
import ProcessTracksOrchestrator from "@components/orchestrator/process-tracks/element.js";
import QueueAudioOrchestrator from "@components/orchestrator/queue-audio/element.js";
import RepeatShuffleEngine from "@components/engine/repeat-shuffle/element.js";
import SearchProcessor from "@components/processor/search/element.js";
import ScopeEngine from "@components/engine/scope/element.js";
import ScopedTracksOrchestrator from "@components/orchestrator/scoped-tracks/element.js";
import SourcesOrchestrator from "@components/orchestrator/sources/element.js";

/**
 * @import { DiffuseElement } from "@toko/diffuse/common/element.js";
 */

const url = new URL(document.location.href);
export const GROUP = url.searchParams.get("group") ?? "facets";

/**
 * Default config for facets.
 */
export const config = {
  GROUP,

  features: {
    fillQueueAutomatically,
    playAudioFromQueue,
    processInputs,
    searchThroughCollection,
  },

  // Elements
  engine: {
    audio,
    queue,
    repeatShuffle,
    scope,
  },
  orchestrator: {
    autoQueue,
    input,
    output,
    queueAudio,
    processTracks,
    scopedTracks,
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

function fillQueueAutomatically() {
  return {
    engine: {
      queue: queue(),
      repeatShuffle: repeatShuffle(),
      scope: scope(),
    },
    orchestrator: {
      autoQueue: autoQueue(),
      input: input(),
      output: output(),
      scopedTracks: scopedTracks(),
    },
  };
}

function playAudioFromQueue() {
  return {
    engine: {
      audio: audio(),
      queue: queue(),
    },
    orchestrator: {
      queueAudio: queueAudio(),
    },
  };
}

function processInputs() {
  return {
    orchestrator: {
      input: input(),
      output: output(),
      processTracks: processTracks(),
    },
    processor: {
      metadata: metadata(),
    },
  };
}

function searchThroughCollection() {
  return {
    engine: {
      scope: scope(),
    },
    orchestrator: {
      output: output(),
      scopedTracks: scopedTracks(),
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

function repeatShuffle() {
  const r = new RepeatShuffleEngine();
  r.setAttribute("group", GROUP);

  return findExistingOrAdd(r);
}

function scope() {
  const s = new ScopeEngine();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s);
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
function autoQueue() {
  const q = queue();
  const r = repeatShuffle();
  const t = scopedTracks();

  const aqo = new AutoQueueOrchestrator();
  aqo.setAttribute("group", GROUP);
  aqo.setAttribute("queue-engine-selector", q.selector);
  aqo.setAttribute("repeat-shuffle-engine-selector", r.selector);
  aqo.setAttribute("tracks-selector", t.selector);

  return findExistingOrAdd(aqo);
}

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

/**
 * @param {Object} opts - Options
 * @param {boolean} [opts.disableWhenReady] - Whether to disable processing when ready.
 */
function processTracks(opts = { disableWhenReady: false }) {
  const i = input();
  const o = output();
  const m = metadata();

  const opt = new ProcessTracksOrchestrator();
  opt.setAttribute("group", GROUP);
  opt.setAttribute("input-selector", i.selector);
  opt.setAttribute("output-selector", o.selector);
  opt.setAttribute("metadata-processor-selector", m.selector);

  if (!opts.disableWhenReady) {
    opt.toggleAttribute("process-when-ready");
  }

  return findExistingOrAdd(opt);
}

function queueAudio() {
  const a = audio();
  const i = input();
  const q = queue();
  const r = repeatShuffle();

  const oqa = new QueueAudioOrchestrator();
  oqa.setAttribute("group", GROUP);
  oqa.setAttribute("audio-engine-selector", a.selector);
  oqa.setAttribute("input-selector", i.selector);
  oqa.setAttribute("queue-engine-selector", q.selector);
  oqa.setAttribute("repeat-shuffle-engine-selector", r.selector);

  return findExistingOrAdd(oqa);
}

function scopedTracks() {
  const i = input();
  const o = output();
  const e = scope();
  const s = search();

  const sto = new ScopedTracksOrchestrator();
  sto.setAttribute("group", GROUP);
  sto.setAttribute("input-selector", i.selector);
  sto.setAttribute("output-selector", o.selector);
  sto.setAttribute("scope-engine-selector", e.selector);
  sto.setAttribute("search-processor-selector", s.selector);

  return findExistingOrAdd(sto);
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
