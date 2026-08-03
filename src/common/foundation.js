import { signal } from "~/common/signal.js";

/**
 * @import { DiffuseElement } from "~/common/element.js";
 * @import { Signal } from "~/common/signal.d.ts";
 * @import { ScrobbleElement } from "@specs/components/supplement/types.d.ts";
 */

const url = new URL(document.location.href);
export const GROUP = url.searchParams.get("group") ?? "facets";

/**
 * [PRIVATE] Signals.
 */
const signals = {
  configurator: {
    artwork: signal(
      /** @type {import("~/components/configurator/artwork/element.js").CLASS | null} */ (null),
    ),
    metadata: signal(
      /** @type {import("~/components/configurator/metadata/element.js").CLASS | null} */ (null),
    ),
    input: signal(
      /** @type {import("~/components/configurator/input/element.js").CLASS | null} */ (null),
    ),
    scrobbles: signal(
      /** @type {import("~/components/configurator/scrobbles/element.js").CLASS | null} */ (null),
    ),
    upload: signal(
      /** @type {import("~/components/configurator/upload/element.js").CLASS | null} */ (null),
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
    artwork: signal(
      /** @type {import("~/components/orchestrator/artwork/element.js").CLASS | null} */ (null),
    ),
    controller: signal(
      /** @type {import("~/components/orchestrator/controller/element.js").CLASS | null} */ (null),
    ),
    coverGroups: signal(
      /** @type {import("~/components/orchestrator/cover-groups/element.js").CLASS | null} */ (null),
    ),
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
    pathCollections: signal(
      /** @type {import("~/components/orchestrator/path-collections/element.js").CLASS | null} */ (null),
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
    spectrogramAudio: signal(
      /** @type {import("~/components/orchestrator/spectrogram-audio/element.js").CLASS | null} */ (null),
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
    artwork: configuratorArtwork,
    metadata: configuratorMetadata,
    input,
    scrobbles,
    upload,
  },

  engine: {
    audio,
    queue,
    repeatShuffle,
    scope,
  },

  orchestrator: {
    artwork,
    controller,
    coverGroups,
    autoQueue,
    favourites,
    mediaSession,
    output,
    pathCollections,
    processTracks,
    queueAudio,
    scopedTracks,
    scrobbleAudio,
    sources,
    spectrogramAudio,
  },

  /**
   * Element signals
   */
  signals: {
    configurator: {
      artwork: signals.configurator.artwork.get,
      metadata: signals.configurator.metadata.get,
      input: signals.configurator.input.get,
      scrobbles: signals.configurator.scrobbles.get,
      upload: signals.configurator.upload.get,
    },

    engine: {
      audio: signals.engine.audio.get,
      queue: signals.engine.queue.get,
      repeatShuffle: signals.engine.repeatShuffle.get,
      scope: signals.engine.scope.get,
    },

    orchestrator: {
      artwork: signals.orchestrator.artwork.get,
      controller: signals.orchestrator.controller.get,
      coverGroups: signals.orchestrator.coverGroups.get,
      autoQueue: signals.orchestrator.autoQueue.get,
      favourites: signals.orchestrator.favourites.get,
      mediaSession: signals.orchestrator.mediaSession.get,
      output: signals.orchestrator.output.get,
      pathCollections: signals.orchestrator.pathCollections.get,
      processTracks: signals.orchestrator.processTracks.get,
      queueAudio: signals.orchestrator.queueAudio.get,
      scopedTracks: signals.orchestrator.scopedTracks.get,
      scrobbleAudio: signals.orchestrator.scrobbleAudio.get,
      sources: signals.orchestrator.sources.get,
      spectrogramAudio: signals.orchestrator.spectrogramAudio.get,
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
  },

  /**
   * Hide the loader and fade in the facet content by adding the `has-loaded`
   * class to `#container` after two animation frames (so the initial opacity:0
   * is painted first and the CSS transition actually runs).
   */
  ready: () => {
    config.hideLoader();

    requestAnimationFrame(() => {
      requestAnimationFrame(() => {
        document.querySelector("#container")?.classList.add("has-loaded");
      });
    });
  },
};

export default config;

// 🥡

// Configurators

async function configuratorArtwork() {
  const { CLASS: ArtworkConfigurator } = await import(
    "~/components/configurator/artwork/element.js"
  );

  const ac = new ArtworkConfigurator();
  ac.setAttribute("group", GROUP);

  return findExistingOrAdd(ac, signals.configurator.artwork);
}

async function configuratorMetadata() {
  const { CLASS: MetadataConfigurator } = await import(
    "~/components/configurator/metadata/element.js"
  );

  const mc = new MetadataConfigurator();
  mc.setAttribute("group", GROUP);

  return findExistingOrAdd(mc, signals.configurator.metadata);
}

async function input() {
  const { CLASS: InputConfigurator } = await import(
    "~/components/configurator/input/element.js"
  );

  const i = new InputConfigurator();
  i.setAttribute("group", GROUP);

  return findExistingOrAdd(i, signals.configurator.input);
}

async function upload() {
  const { CLASS: UploadConfigurator } = await import(
    "~/components/configurator/upload/element.js"
  );

  const u = new UploadConfigurator();
  u.setAttribute("group", GROUP);

  return findExistingOrAdd(u, signals.configurator.upload);
}

/**
 * @returns {Promise<ScrobbleElement>}
 */
async function scrobbles() {
  const { CLASS: ScrobblesConfigurator } = await import(
    "~/components/configurator/scrobbles/element.js"
  );

  const sc = new ScrobblesConfigurator();
  sc.setAttribute("group", GROUP);

  const existing = document.body.querySelector(sc.selector);

  if (existing) {
    return /** @type {ScrobbleElement} */ (existing);
  }

  document.body.append(sc);
  return /** @type {ScrobbleElement} */ (/** @type {unknown} */ (sc));
}

// Engines
async function audio() {
  const { CLASS: AudioEngine } = await import(
    "~/components/engine/audio/element.js"
  );

  const a = new AudioEngine();
  a.setAttribute("group", GROUP);

  return findExistingOrAdd(a, signals.engine.audio);
}

async function queue() {
  const { CLASS: Queue } = await import(
    "~/components/engine/queue/element.js"
  );

  const q = new Queue();
  q.setAttribute("group", GROUP);

  return findExistingOrAdd(q, signals.engine.queue);
}

async function repeatShuffle() {
  const { CLASS: RepeatShuffleEngine } = await import(
    "~/components/engine/repeat-shuffle/element.js"
  );

  const r = new RepeatShuffleEngine();
  r.setAttribute("group", GROUP);

  return findExistingOrAdd(r, signals.engine.repeatShuffle);
}

async function scope() {
  const { CLASS: ScopeEngine } = await import(
    "~/components/engine/scope/element.js"
  );

  const s = new ScopeEngine();
  s.setAttribute("group", GROUP);

  return findExistingOrAdd(s, signals.engine.scope);
}

// Orchestrators

async function artwork() {
  const [{ CLASS: ArtworkOrchestrator }, ac] = await Promise.all([
    import("~/components/orchestrator/artwork/element.js"),
    configuratorArtwork(),
  ]);

  const a = new ArtworkOrchestrator();
  a.setAttribute("group", GROUP);
  a.setAttribute("artwork-selector", ac.selector);

  return findExistingOrAdd(a, signals.orchestrator.artwork);
}

async function autoQueue() {
  const [{ CLASS: AutoQueueOrchestrator }, q, r, t] = await Promise.all([
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

async function controller() {
  const [{ CLASS: ControllerOrchestrator }, a, o, q] = await Promise.all([
    import("~/components/orchestrator/controller/element.js"),
    audio(),
    output(),
    queue(),
  ]);

  const co = new ControllerOrchestrator();
  co.setAttribute("audio-engine-selector", a.selector);
  co.setAttribute("output-selector", o.selector);
  co.setAttribute("queue-engine-selector", q.selector);

  return findExistingOrAdd(co, signals.orchestrator.controller);
}

async function coverGroups() {
  const [{ CLASS: CoverGroupsOrchestrator }, t] = await Promise.all([
    import("~/components/orchestrator/cover-groups/element.js"),
    scopedTracks(),
  ]);

  const cgo = new CoverGroupsOrchestrator();
  cgo.setAttribute("tracks-selector", t.selector);

  return findExistingOrAdd(cgo, signals.orchestrator.coverGroups);
}

async function favourites() {
  const [{ CLASS: FavouritesOrchestrator }, o] = await Promise.all([
    import("~/components/orchestrator/favourites/element.js"),
    output(),
  ]);

  const fo = new FavouritesOrchestrator();
  fo.setAttribute("group", GROUP);
  fo.setAttribute("output-selector", o.selector);

  return findExistingOrAdd(fo, signals.orchestrator.favourites);
}

async function mediaSession() {
  const [{ CLASS: MediaSessionOrchestrator }, a, aw, o, q] = await Promise
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
  mso.setAttribute("artwork-selector", aw.selector);
  mso.setAttribute("output-selector", o.selector);
  mso.setAttribute("queue-engine-selector", q.selector);

  return findExistingOrAdd(mso, signals.orchestrator.mediaSession);
}

/**
 * @param {Object} [options] - Options
 * @param {string} [options.namespace] - The namespace to use for the output.
 */
async function output(options) {
  const { CLASS: OutputOrchestrator } = await import(
    "~/components/orchestrator/output/element.js"
  );

  const o = new OutputOrchestrator();
  o.setAttribute("group", GROUP);

  if (options?.namespace) o.setAttribute("namespace", options.namespace);

  return findExistingOrAdd(o, signals.orchestrator.output);
}

/**
 * @param {Object} opts - Options
 * @param {boolean} [opts.disableWhenReady] - Whether to disable processing when ready.
 */
async function processTracks(opts = { disableWhenReady: false }) {
  const [{ CLASS: ProcessTracksOrchestrator }, i, o, m] = await Promise.all([
    import("~/components/orchestrator/process-tracks/element.js"),
    input(),
    output(),
    configuratorMetadata(),
  ]);

  const opt = new ProcessTracksOrchestrator();
  opt.setAttribute("group", GROUP);
  opt.setAttribute("input-selector", i.selector);
  opt.setAttribute("output-selector", o.selector);
  opt.setAttribute("metadata-selector", m.selector);

  if (!opts.disableWhenReady) {
    opt.toggleAttribute("process-when-ready");
  }

  return findExistingOrAdd(opt, signals.orchestrator.processTracks);
}

async function queueAudio() {
  const [{ CLASS: QueueAudioOrchestrator }, a, i, o, q, r] = await Promise
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
  const [{ CLASS: ScopedTracksOrchestrator }, i, o, e] = await Promise.all([
    import("~/components/orchestrator/scoped-tracks/element.js"),
    input(),
    output(),
    scope(),
  ]);

  const sto = new ScopedTracksOrchestrator();
  sto.setAttribute("group", GROUP);
  sto.setAttribute("input-selector", i.selector);
  sto.setAttribute("output-selector", o.selector);
  sto.setAttribute("scope-engine-selector", e.selector);

  return findExistingOrAdd(sto, signals.orchestrator.scopedTracks);
}

async function scrobbleAudio() {
  const [{ CLASS: ScrobbleAudioOrchestrator }, a, sc] = await Promise.all([
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

async function spectrogramAudio() {
  const [
    { CLASS: SpectrogramAudioOrchestrator },
    { CLASS: SpectrogramMetadata },
    a,
    i,
    o,
  ] = await Promise.all([
    import("~/components/orchestrator/spectrogram-audio/element.js"),
    import("~/components/metadata/spectrogram/element.js"),
    audio(),
    input(),
    output(),
  ]);

  // A standalone spectrogram metadata element the orchestrator can call
  // directly. It's given the input configurator so its worker can fetch audio.
  const sm = new SpectrogramMetadata();
  sm.setAttribute("group", GROUP);
  sm.setAttribute("input-selector", i.selector);

  if (!document.body.querySelector(sm.selector)) {
    document.body.append(sm);
  }

  const sao = new SpectrogramAudioOrchestrator();
  sao.setAttribute("group", GROUP);
  sao.setAttribute("audio-engine-selector", a.selector);
  sao.setAttribute("spectrogram-selector", sm.selector);
  sao.setAttribute("output-selector", o.selector);

  return findExistingOrAdd(sao, signals.orchestrator.spectrogramAudio);
}

async function pathCollections() {
  const [{ CLASS: PathCollectionsOrchestrator }, o] = await Promise.all([
    import("~/components/orchestrator/path-collections/element.js"),
    output(),
  ]);

  const pco = new PathCollectionsOrchestrator();
  pco.setAttribute("group", GROUP);
  pco.setAttribute("output-selector", o.selector);

  return findExistingOrAdd(pco, signals.orchestrator.pathCollections);
}

async function sources() {
  const [{ CLASS: SourcesOrchestrator }, i, o] = await Promise.all([
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
