/**
 * @import { DiffuseElement } from "~/common/element.js";
 * @import { ScrobbleElement } from "~/components/supplement/types.d.ts";
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
  configurator: {
    scrobbles,
  },
  engine: {
    audio,
    queue,
    repeatShuffle,
    scope,
  },
  orchestrator: {
    autoQueue,
    favourites,
    input,
    mediaSession,
    output,
    queueAudio,
    processTracks,
    scopedTracks,
    scrobbleAudio,
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

async function fillQueueAutomatically() {
  const [q, rs, sc, aq, i, o, st] = await Promise.all([
    // engine
    queue(),
    repeatShuffle(),
    scope(),

    // orchestrator
    autoQueue(),
    input(),
    output(),
    scopedTracks(),
  ]);

  return {
    engine: {
      queue: q,
      repeatShuffle: rs,
      scope: sc,
    },
    orchestrator: {
      autoQueue: aq,
      input: i,
      output: o,
      scopedTracks: st,
    },
  };
}

async function playAudioFromQueue() {
  const [sc, a, q, ms, qa, sca] = await Promise.all([
    // configurator
    scrobbles(),

    // engine
    audio(),
    queue(),

    // orchestrator
    mediaSession(),
    queueAudio(),
    scrobbleAudio(),
  ]);

  return {
    configurator: {
      scrobbles: sc,
    },
    engine: {
      audio: a,
      queue: q,
    },
    orchestrator: {
      mediaSession: ms,
      queueAudio: qa,
      scrobbleAudio: sca,
    },
  };
}

async function processInputs() {
  const [i, o, pt, m] = await Promise.all([
    // orchestrator
    input(),
    output(),
    processTracks(),

    // processor
    metadata(),
  ]);

  return {
    orchestrator: {
      input: i,
      output: o,
      processTracks: pt,
    },
    processor: {
      metadata: m,
    },
  };
}

async function searchThroughCollection() {
  const [sc, o, st, s] = await Promise.all([
    // engine
    scope(),

    // orchestrator
    output(),
    scopedTracks(),

    // processor
    search(),
  ]);

  return {
    engine: {
      scope: sc,
    },
    orchestrator: {
      output: o,
      scopedTracks: st,
    },
    processor: {
      search: s,
    },
  };
}

// 🥡

// Configurators

/**
 * @returns {Promise<ScrobbleElement>}
 */
async function scrobbles() {
  const [{ default: ScrobblesConfigurator }, { default: LastFmScrobbler }] =
    await Promise.all([
      import("~/components/configurator/scrobbles/element.js"),
      import("~/components/supplement/last.fm/element.js"),
    ]);

  const sc = new ScrobblesConfigurator();
  sc.setAttribute("group", GROUP);
  sc.setAttribute("id", "scrobbles");

  const existing = document.body.querySelector(sc.selector);

  if (existing) {
    return /** @type {ScrobbleElement} */ (existing);
  }

  const lastFm = new LastFmScrobbler();
  lastFm.setAttribute("group", GROUP);

  sc.append(lastFm);

  document.body.append(sc);
  return /** @type {ScrobbleElement} */ (/** @type {unknown} */ (sc));
}

// Engines
async function audio() {
  const { default: AudioEngine } = await import(
    "~/components/engine/audio/element.js"
  );

  const a = new AudioEngine();
  a.setAttribute("group", GROUP);

  return findExistingOrAdd(a);
}

async function queue() {
  const { default: Queue } = await import(
    "~/components/engine/queue/element.js"
  );

  const q = new Queue();
  q.setAttribute("group", GROUP);

  return findExistingOrAdd(q);
}

async function repeatShuffle() {
  const { default: RepeatShuffleEngine } = await import(
    "~/components/engine/repeat-shuffle/element.js"
  );

  const r = new RepeatShuffleEngine();
  r.setAttribute("group", GROUP);

  return findExistingOrAdd(r);
}

async function scope() {
  const { default: ScopeEngine } = await import(
    "~/components/engine/scope/element.js"
  );

  const s = new ScopeEngine();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s);
}

// Processors
async function artwork() {
  const { default: ArtworkProcessor } = await import(
    "~/components/processor/artwork/element.js"
  );

  const a = new ArtworkProcessor();
  a.setAttribute("group", GROUP);

  return findExistingOrAdd(a);
}

async function metadata() {
  const { default: MetadataProcessor } = await import(
    "~/components/processor/metadata/element.js"
  );

  const m = new MetadataProcessor();
  m.setAttribute("group", GROUP);

  return findExistingOrAdd(m);
}

async function search() {
  const { default: SearchProcessor } = await import(
    "~/components/processor/search/element.js"
  );

  const s = new SearchProcessor();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s);
}

// Orchestrators
async function autoQueue() {
  const [{ default: AutoQueueOrchestrator }, q, r, t] = await Promise.all([
    import("~/components/orchestrator/auto-queue/element.js"),
    queue(),
    repeatShuffle(),
    scopedTracks(),
  ]);

  const aqo = new AutoQueueOrchestrator();
  aqo.setAttribute("group", GROUP);
  aqo.setAttribute("queue-engine-selector", q.selector);
  aqo.setAttribute("repeat-shuffle-engine-selector", r.selector);
  aqo.setAttribute("tracks-selector", t.selector);

  return findExistingOrAdd(aqo);
}

async function favourites() {
  const [{ default: FavouritesOrchestrator }, o] = await Promise.all([
    import("~/components/orchestrator/favourites/element.js"),
    output(),
  ]);

  const fo = new FavouritesOrchestrator();
  fo.setAttribute("group", GROUP);
  fo.setAttribute("output-selector", o.selector);

  return findExistingOrAdd(fo);
}

async function input() {
  const { default: InputOrchestrator } = await import(
    "~/components/orchestrator/input/element.js"
  );

  const i = new InputOrchestrator();
  i.setAttribute("group", GROUP);
  i.setAttribute("id", "input");

  return findExistingOrAdd(i);
}

async function mediaSession() {
  const [{ default: MediaSessionOrchestrator }, a, aw, o, q] = await Promise
    .all([
      import("~/components/orchestrator/media-session/element.js"),
      audio(),
      artwork(),
      output(),
      queue(),
    ]);

  const mso = new MediaSessionOrchestrator();
  mso.setAttribute("group", GROUP);
  mso.setAttribute("audio-engine-selector", a.selector);
  mso.setAttribute("artwork-processor-selector", aw.selector);
  mso.setAttribute("output-selector", o.selector);
  mso.setAttribute("queue-engine-selector", q.selector);

  return findExistingOrAdd(mso);
}

async function output() {
  const { default: OutputOrchestrator } = await import(
    "~/components/orchestrator/output/element.js"
  );

  const o = new OutputOrchestrator();
  o.setAttribute("group", GROUP);
  o.setAttribute("id", "output");

  return findExistingOrAdd(o);
}

/**
 * @param {Object} opts - Options
 * @param {boolean} [opts.disableWhenReady] - Whether to disable processing when ready.
 */
async function processTracks(opts = { disableWhenReady: false }) {
  const [{ default: ProcessTracksOrchestrator }, i, o, m] = await Promise.all([
    import("~/components/orchestrator/process-tracks/element.js"),
    input(),
    output(),
    metadata(),
  ]);

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

async function queueAudio() {
  const [{ default: QueueAudioOrchestrator }, a, i, o, q, r] = await Promise
    .all([
      import("~/components/orchestrator/queue-audio/element.js"),
      audio(),
      input(),
      output(),
      queue(),
      repeatShuffle(),
    ]);

  const oqa = new QueueAudioOrchestrator();
  oqa.setAttribute("group", GROUP);
  oqa.setAttribute("audio-engine-selector", a.selector);
  oqa.setAttribute("input-selector", i.selector);
  oqa.setAttribute("output-selector", o.selector);
  oqa.setAttribute("queue-engine-selector", q.selector);
  oqa.setAttribute("repeat-shuffle-engine-selector", r.selector);

  return findExistingOrAdd(oqa);
}

async function scopedTracks() {
  const [{ default: ScopedTracksOrchestrator }, i, o, e, s] = await Promise.all(
    [
      import("~/components/orchestrator/scoped-tracks/element.js"),
      input(),
      output(),
      scope(),
      search(),
    ],
  );

  const sto = new ScopedTracksOrchestrator();
  sto.setAttribute("group", GROUP);
  sto.setAttribute("input-selector", i.selector);
  sto.setAttribute("output-selector", o.selector);
  sto.setAttribute("scope-engine-selector", e.selector);
  sto.setAttribute("search-processor-selector", s.selector);

  return findExistingOrAdd(sto);
}

async function scrobbleAudio() {
  const [{ default: ScrobbleAudioOrchestrator }, a, sc] = await Promise.all([
    import("~/components/orchestrator/scrobble-audio/element.js"),
    audio(),
    scrobbles(),
  ]);

  const sao = new ScrobbleAudioOrchestrator();
  sao.setAttribute("group", GROUP);
  sao.setAttribute("audio-engine-selector", a.selector);
  sao.setAttribute("scrobble-selector", sc.selector);

  return findExistingOrAdd(sao);
}

async function sources() {
  const [{ default: SourcesOrchestrator }, i, o] = await Promise.all([
    import("~/components/orchestrator/sources/element.js"),
    input(),
    output(),
  ]);

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
