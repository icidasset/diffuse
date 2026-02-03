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
  // Pre-instantiated
  const instantiated = {
    engine: {
      queue: queue(),
    },
    orchestrator: {
      input: input(),
      output: output(),
      queueTracks: queueTracks(),
      processTracks: processTracks(),
      repeatShuffle: repeatShuffle(),
    },
    processor: {
      metadata: metadata(),
    },
  };

  // Return elements
  return {
    GROUP,

    instantiated,

    lazy: {
      engine: {
        audio,
      },
      orchestrator: {
        queueAudio,
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

// 🥡

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

function metadata() {
  const m = new MetadataProcessor();
  m.setAttribute("group", GROUP);

  addToBodyIfNeeded(m);
  return m;
}

function search() {
  const s = new SearchProcessor();
  s.setAttribute("group", GROUP);

  addToBodyIfNeeded(s);
  return s;
}

// Orchestrators
function input() {
  const i = new InputOrchestrator();
  i.setAttribute("group", GROUP);
  i.setAttribute("id", "input");

  addToBodyIfNeeded(i);
  return i;
}

function output() {
  const o = new OutputOrchestrator();
  o.setAttribute("group", GROUP);
  o.setAttribute("id", "output");

  addToBodyIfNeeded(o);
  return o;
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

  document.body.append(opt);
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

  addToBodyIfNeeded(oqa);
  return oqa;
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
  const i = input();
  const o = output();
  const s = search();

  const ost = new SearchTracksOrchestrator();
  ost.setAttribute("group", GROUP);
  ost.setAttribute("input-selector", i.selector);
  ost.setAttribute("output-selector", o.selector);
  ost.setAttribute("search-processor-selector", s.selector);

  addToBodyIfNeeded(ost);
  return ost;
}

function sources() {
  const i = input();
  const o = output();
  const so = new SourcesOrchestrator();
  so.setAttribute("group", GROUP);
  so.setAttribute("input-selector", i.selector);
  so.setAttribute("output-selector", o.selector);

  addToBodyIfNeeded(so);
  return so;
}

// 🛠️

/**
 * @param {DiffuseElement} element
 */
export function addToBodyIfNeeded(element) {
  const alreadyAdded = document.body.querySelector(element.selector);
  if (!alreadyAdded) document.body.append(element);
}
