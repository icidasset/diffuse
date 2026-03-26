import { signal } from "~/common/signal.js";

/**
 * @import { DiffuseElement } from "~/common/element.js";
 * @import { Signal } from "~/common/signal.d.ts";
 * @import { ScrobbleElement } from "~/components/supplement/types.d.ts";
 */

const url = new URL(document.location.href);
export const GROUP = url.searchParams.get("group") ?? "facets";

/**
 * [PRIVATE] Signals.
 */
const signals = {
  configurator: {
    input: signal(
      /** @type {import("~/components/configurator/input/element.js").CLASS | null} */ (null),
    ),
    scrobbles: signal(
      /** @type {import("~/components/configurator/scrobbles/element.js").CLASS | null} */ (null),
    ),
  },

  engine: {
    audio: signal(
      /** @type {import("~/components/engine/audio/element.js").CLASS | null} */ (null),
    ),
    queue: signal(
      /** @type {import("~/components/engine/queue/element.js").CLASS | null} */ (null),
    ),
    repeatShuffle: signal(
      /** @type {import("~/components/engine/repeat-shuffle/element.js").CLASS | null} */ (null),
    ),
    scope: signal(
      /** @type {import("~/components/engine/scope/element.js").CLASS | null} */ (null),
    ),
  },

  orchestrator: {
    autoQueue: signal(
      /** @type {import("~/components/orchestrator/auto-queue/element.js").CLASS | null} */ (null),
    ),
    favourites: signal(
      /** @type {import("~/components/orchestrator/favourites/element.js").CLASS | null} */ (null),
    ),
    mediaSession: signal(
      /** @type {import("~/components/orchestrator/media-session/element.js").CLASS | null} */ (null),
    ),
    output: signal(
      /** @type {import("~/components/orchestrator/output/element.js").CLASS | null} */ (null),
    ),
    processTracks: signal(
      /** @type {import("~/components/orchestrator/process-tracks/element.js").CLASS | null} */ (null),
    ),
    queueAudio: signal(
      /** @type {import("~/components/orchestrator/queue-audio/element.js").CLASS | null} */ (null),
    ),
    scopedTracks: signal(
      /** @type {import("~/components/orchestrator/scoped-tracks/element.js").CLASS | null} */ (null),
    ),
    scrobbleAudio: signal(
      /** @type {import("~/components/orchestrator/scrobble-audio/element.js").CLASS | null} */ (null),
    ),
    sources: signal(
      /** @type {import("~/components/orchestrator/sources/element.js").CLASS | null} */ (null),
    ),
  },

  processor: {
    artwork: signal(
      /** @type {import("~/components/processor/artwork/element.js").CLASS | null} */ (null),
    ),
    metadata: signal(
      /** @type {import("~/components/processor/metadata/element.js").CLASS | null} */ (null),
    ),
    search: signal(
      /** @type {import("~/components/processor/search/element.js").CLASS | null} */ (null),
    ),
  },
};

/**
 * Default config for facets.
 */
export const config = {
  GROUP,

  // Elements
  configurator: {
    input,
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
    mediaSession,
    output,
    queueAudio,
    // TODO: Path collections orchestrator
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

  /**
   * Element signals
   */
  signals: {
    configurator: {
      input: signals.configurator.input.get,
      scrobbles: signals.configurator.scrobbles.get,
    },

    engine: {
      audio: signals.engine.audio.get,
      queue: signals.engine.queue.get,
      repeatShuffle: signals.engine.repeatShuffle.get,
      scope: signals.engine.scope.get,
    },

    orchestrator: {
      autoQueue: signals.orchestrator.autoQueue.get,
      favourites: signals.orchestrator.favourites.get,
      mediaSession: signals.orchestrator.mediaSession.get,
      output: signals.orchestrator.output.get,
      processTracks: signals.orchestrator.processTracks.get,
      queueAudio: signals.orchestrator.queueAudio.get,
      scopedTracks: signals.orchestrator.scopedTracks.get,
      scrobbleAudio: signals.orchestrator.scrobbleAudio.get,
      sources: signals.orchestrator.sources.get,
    },

    processor: {
      artwork: signals.processor.artwork.get,
      metadata: signals.processor.metadata.get,
      search: signals.processor.search.get,
    },
  },

  // Utilities

  container: () => {
    return document.body.querySelector("#container");
  },

  hideLoader: () => {
    const loader = document.querySelector("#diffuse-loader");

    if (loader) {
      loader.classList.add("loaded");
      setTimeout(() => {
        loader.remove();
        loader.parentElement?.remove();
      }, 750);
    }
  },

  /**
   * @param {{ title: string }} options
   */
  setup: ({ title }) => {
    document.title = title;
    config.hideLoader();
  },
};

export default config;

// 🥡

// Configurators

async function input() {
  const { default: InputConfigurator } = await import(
    "~/components/configurator/input/element.js"
  );

  const i = new InputConfigurator();
  i.setAttribute("group", GROUP);
  i.setAttribute("id", "input");

  return findExistingOrAdd(i, signals.configurator.input);
}

/**
 * @returns {Promise<ScrobbleElement>}
 */
async function scrobbles() {
  const { default: ScrobblesConfigurator } = await import(
    "~/components/configurator/scrobbles/element.js"
  );

  const sc = new ScrobblesConfigurator();
  sc.setAttribute("group", GROUP);
  sc.setAttribute("id", "scrobbles");

  const existing = document.body.querySelector(sc.selector);

  if (existing) {
    return /** @type {ScrobbleElement} */ (existing);
  }

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

  return findExistingOrAdd(a, signals.engine.audio);
}

async function queue() {
  const { default: Queue } = await import(
    "~/components/engine/queue/element.js"
  );

  const q = new Queue();
  q.setAttribute("group", GROUP);

  return findExistingOrAdd(q, signals.engine.queue);
}

async function repeatShuffle() {
  const { default: RepeatShuffleEngine } = await import(
    "~/components/engine/repeat-shuffle/element.js"
  );

  const r = new RepeatShuffleEngine();
  r.setAttribute("group", GROUP);

  return findExistingOrAdd(r, signals.engine.repeatShuffle);
}

async function scope() {
  const { default: ScopeEngine } = await import(
    "~/components/engine/scope/element.js"
  );

  const s = new ScopeEngine();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s, signals.engine.scope);
}

// Processors
async function artwork() {
  const { default: ArtworkProcessor } = await import(
    "~/components/processor/artwork/element.js"
  );

  const a = new ArtworkProcessor();
  a.setAttribute("group", GROUP);

  return findExistingOrAdd(a, signals.processor.artwork);
}

async function metadata() {
  const { default: MetadataProcessor } = await import(
    "~/components/processor/metadata/element.js"
  );

  const m = new MetadataProcessor();
  m.setAttribute("group", GROUP);

  return findExistingOrAdd(m, signals.processor.metadata);
}

async function search() {
  const { default: SearchProcessor } = await import(
    "~/components/processor/search/element.js"
  );

  const s = new SearchProcessor();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s, signals.processor.search);
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

  return findExistingOrAdd(aqo, signals.orchestrator.autoQueue);
}

async function favourites() {
  const [{ default: FavouritesOrchestrator }, o] = await Promise.all([
    import("~/components/orchestrator/favourites/element.js"),
    output(),
  ]);

  const fo = new FavouritesOrchestrator();
  fo.setAttribute("group", GROUP);
  fo.setAttribute("output-selector", o.selector);

  return findExistingOrAdd(fo, signals.orchestrator.favourites);
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

  return findExistingOrAdd(mso, signals.orchestrator.mediaSession);
}

async function output() {
  const { default: OutputOrchestrator } = await import(
    "~/components/orchestrator/output/element.js"
  );

  const o = new OutputOrchestrator();
  o.setAttribute("group", GROUP);
  o.setAttribute("id", "output");

  return findExistingOrAdd(o, signals.orchestrator.output);
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

  return findExistingOrAdd(opt, signals.orchestrator.processTracks);
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

  return findExistingOrAdd(oqa, signals.orchestrator.queueAudio);
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

  return findExistingOrAdd(sto, signals.orchestrator.scopedTracks);
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

  return findExistingOrAdd(sao, signals.orchestrator.scrobbleAudio);
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

  return findExistingOrAdd(so, signals.orchestrator.sources);
}

// 🛠️

/**
 * @template {DiffuseElement} T
 * @param {T} element
 * @param {Signal<T | null>} signal
 * @returns {T}
 */
export function findExistingOrAdd(element, signal) {
  /** @type {T | null} */
  const alreadyAdded = document.body.querySelector(element.selector);

  if (!alreadyAdded) {
    document.body.append(element);
    signal.value = element;
    return element;
  }

  signal.value = alreadyAdded;
  return alreadyAdded;
}
