import { keyed } from "lit-html/directives/keyed.js";

import {
  BroadcastableDiffuseElement,
  defineElement,
  nothing,
} from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";

/**
 * @import {Actions, AudioUrl, AudioState, AudioStateReadOnly, LoadingState} from "@specs/components/engine/audio/types.d.ts"
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {SignalReader} from "~/common/signal.d.ts"
 */

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////
/**
 * Mobile Safari caps the number of live media elements and behaves poorly
 * when fresh <audio> nodes are created on every track change. On iOS we
 * therefore render a single <audio> element and reuse its DOM node across
 * track switches instead of keying one per item id.
 */
const IS_IOS = /iPhone|iPad|iPod/.test(navigator.userAgent) ||
  (navigator.platform === "MacIntel" && navigator.maxTouchPoints > 1);

/**
 * Module-internal Web Audio routing hooks. Accessed only by {@link AudioEngine}
 * and {@link AudioEngineItem} (both live in this file), so these stay out of
 * the public surface — external consumers use `engine.webAudio` instead.
 *
 * @type {unique symbol}
 */
const ROUTE_AUDIO = Symbol("routeAudio");
/** @type {unique symbol} */
const UNROUTE_AUDIO = Symbol("unrouteAudio");

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * @implements {Actions}
 */
class AudioEngine extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/audio";

  constructor() {
    super();

    this.state = this.state.bind(this);
  }

  /** @type {Map<string, string>} MediaSource object URLs created from streams, keyed by item ID */
  #mediaSourceUrls = new Map();

  /** @type {Map<string, ReadableStream>} Streams pending MediaSource setup */
  #streams = new Map();

  /** Aborts in-flight MediaSource setup when the element is disconnected. */
  #streamAbort = new AbortController();

  // WEB AUDIO
  //
  // Every <audio> element is routed through a shared AudioContext so consumers
  // (themes, equalizer / visualizer plugins, …) can tap into the signal. All
  // sources land on a single post-volume `input` node that is connected to the
  // destination by default; a consumer replaces that edge with its own chain
  // (e.g. `input → biquadFilter → destination`) to apply DSP.
  //
  // Chain (unless a consumer splices its nodes in):
  //   mediaElementSource -> input -> destination

  /** @type {AudioContext | undefined} Lazily created, shared by all items. */
  #audioContext = undefined;
  /** @type {GainNode | undefined} Post-volume tap point that all sources feed. */
  #input = undefined;
  /** @type {Map<HTMLAudioElement, MediaElementAudioSourceNode>} */
  #sourceNodes = new Map();

  // SIGNALS

  #items = signal(/** @type {AudioUrl[]} */ ([]));
  #volume = signal(0.75);

  // STATE

  items = this.#items.get;
  volume = this.#volume.get;

  isPlaying = computed(() => {
    const item = this.items()?.[0];
    if (!item) return false;

    const state = this.state(item.id);
    if (!state) return false;

    return state.isPlaying() || state.hasEnded() ||
      (state.duration() > 0 && state.currentTime() === state.duration());
  });

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    // Reset teardown signal in case the element is reconnected (moved in DOM).
    this.#streamAbort = new AbortController();

    // Setup broadcasting if part of group
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(
        this.identifier,
        {
          adjustVolume: { strategy: "replicate", fn: this.adjustVolume },
          pause: { strategy: "leaderOnly", fn: this.pause },
          play: { strategy: "leaderOnly", fn: this.play },
          seek: { strategy: "leaderOnly", fn: this.seek },
          supply: { strategy: "replicate", fn: this.supply },

          // State
          items: { strategy: "leaderOnly", fn: this.items },
        },
      );

      if (!actions) return;

      this.adjustVolume = actions.adjustVolume;
      this.pause = actions.pause;
      this.play = actions.play;
      this.seek = actions.seek;
      this.supply = actions.supply;

      // Sync items with leader if needed
      this.broadcastingStatus().then(async (status) => {
        if (status.leader) return;
        this.#items.value = await actions.items();
      });
    }

    // Super
    super.connectedCallback();

    // Get volume from previous session if possible
    const VOLUME_KEY =
      `${this.constructor.prototype.constructor.NAME}/${this.group}/volume`;
    const volume = localStorage.getItem(VOLUME_KEY);

    if (volume != undefined) {
      this.#volume.set(parseFloat(volume));
    }

    // Monitor volume signal
    this.effect(() => {
      // Master volume is applied through the Web Audio graph's input gain node.
      // When the graph hasn't been created yet (e.g. no AudioContext support) we
      // fall back to setting the volume directly on each element.
      if (this.#input) {
        this.#input.gain.value = this.#volume.value;
      }

      Array.from(this.querySelectorAll("de-audio-item")).forEach(
        (node) => {
          const item = /** @type {AudioEngineItem} */ (node);
          if (item.hasAttribute("preload")) return;
          const audio = item.querySelector("audio");
          if (audio && !this.#sourceNodes.has(audio)) {
            audio.volume = this.#volume.value;
          }
        },
      );

      localStorage.setItem(VOLUME_KEY, this.#volume.value.toString());
    });

    // iOS: resume playback that silently failed to start while the page was
    // hidden (mobile Safari suspends media loading & the audio session in
    // the background). If playback was requested but the element is paused,
    // replay it now that the page is visible — reloading first if nothing
    // was buffered yet.
    if (IS_IOS) {
      const onVisible = () => {
        if (document.hidden) return;

        this.items().forEach((item) => {
          if (item.isPreload) return;

          const el = this.#itemElement(item.id);
          if (!el?.intendsToPlay) return;

          const audio = el.audio;
          if (!audio.paused) return;

          if (audio.readyState >= 2) {
            this.play({ audioId: item.id });
          } else {
            audio.load();
            audio.addEventListener("canplay", () => {
              if (el.intendsToPlay) this.play({ audioId: item.id });
            }, { once: true });
          }
        });
      };

      this.effect(() => {
        document.addEventListener("visibilitychange", onVisible);
        return () =>
          document.removeEventListener("visibilitychange", onVisible);
      });
    }

    // Only broadcasting stuff from here on out
    if (!this.broadcasted) return;

    // Manage playback across tabs if needed
    this.effect(async () => {
      const status = await this.broadcastingStatus();
      untracked(() => {
        if (!(status.leader && status.initialLeader === false)) return;

        console.log("🧙 Leadership acquired");
        this.items().forEach((item) => {
          const el = this.#itemElement(item.id);
          if (!el) return;

          el.removeAttribute("initial-progress");

          if (!el.audio) return;

          const currentTime = el.$state.currentTime.value;
          const canPlay = () => {
            this.seek({
              audioId: item.id,
              currentTime: currentTime,
            });

            if (el.$state.isPlaying.value) this.play({ audioId: item.id });
          };

          el.audio.addEventListener("canplay", canPlay, { once: true });

          if (el.audio.readyState === 0) el.audio.load();
          else canPlay();
        });
      });
    });
  }

  /**
   * @override
   */
  disconnectedCallback() {
    // Abort in-flight MediaSource setup so #resolveStream can wind down even
    // while it's awaiting `sourceopen` (which may otherwise never fire once
    // the object URL is revoked / the element is detached).
    this.#streamAbort.abort();

    // Revoke every MediaSource object URL. WebKit refcounts these and only
    // frees the buffered data on revokeObjectURL — dropping the map without
    // revoking leaks the decoded track bytes until the tab's process dies.
    for (const objectUrl of this.#mediaSourceUrls.values()) {
      URL.revokeObjectURL(objectUrl);
    }
    this.#mediaSourceUrls.clear();

    // Cancel pending (not-yet-resolved) streams so their underlying sources
    // (e.g. fetches) release immediately instead of draining forever.
    for (const stream of this.#streams.values()) {
      stream.cancel().catch(() => {});
    }
    this.#streams.clear();

    // Stop and unload any live audio nodes before they're dropped so detached
    // media doesn't keep playing / holding the audio session.
    this.querySelectorAll("de-audio-item").forEach((node) => {
      const item = /** @type {AudioEngineItem} */ (node);
      let audio;
      try {
        audio = item.audio; // throws when there's no child <audio>
      } catch {
        return;
      }
      audio.pause();
      audio.removeAttribute("src");
      item.querySelectorAll("source").forEach((s) => s.removeAttribute("src"));
      audio.load();
    });

    // Detach the <audio> source nodes and close the shared AudioContext.
    this.#teardownWebAudio();

    super.disconnectedCallback();
  }

  // ACTIONS

  /**
   * @type {Actions["adjustVolume"]}
   */
  adjustVolume(args) {
    if (args.audioId) {
      this.#withAudioNode(args.audioId, (audio) => {
        audio.volume = args.volume;
      });
    } else {
      this.#volume.value = args.volume;
    }
  }

  /**
   * @type {Actions["pause"]}
   */
  pause({ audioId }) {
    this.#withAudioNode(audioId, (audio, item) => {
      audio.pause();
      item.intendsToPlay = false;
      // Set `isPlaying` to false optimistically, mirroring `play()`. The
      // `pause` event would normally do this via `pauseEvent`, but when
      // `play()` was called before the audio had buffered enough to start
      // (e.g. readyState < HAVE_FUTURE_DATA) the browser may never fire a
      // `pause` event — leaving `isPlaying` stuck on the optimistic `true`
      // that `play()` set. It also prevents `canplayEvent`'s retry-on-ready
      // logic from restarting playback after an explicit pause.
      item.$state.isPlaying.set(false);
    });
  }

  /**
   * @type {Actions["play"]}
   */
  play({ audioId, volume }) {
    this.#resumeContext();

    this.#withAudioNode(audioId, (audio, item) => {
      // Routed elements get master volume from the graph's gain node, so keep
      // their own volume at unity unless a per-item override is given. In the
      // no-Web-Audio fallback the element's volume carries the master level.
      const routed = this.#sourceNodes.has(audio);
      audio.volume = volume ?? (routed ? 1 : this.volume());
      audio.muted = false;

      // TODO: Might need this for `data-initial-progress`
      //       Does seem to cause trouble when broadcasting
      //       (open multiple sessions and play the next audio)
      // if (audio.readyState === 0) audio.load();
      if (!audio.isConnected) return;

      const promise = audio.play() || Promise.resolve();
      item.intendsToPlay = true;

      // On iOS a backgrounded play() can resolve without playback ever
      // starting — mobile Safari suspends the load (and the audio session)
      // until the page is visible again. Don't claim it's playing in that
      // case: `playEvent`/`playingEvent` set the state if playback truly
      // starts, and the media session won't show a phantom progress. The
      // visibilitychange handler (see connectedCallback) resumes playback
      // on refocus using `intendsToPlay`.
      if (!(IS_IOS && document.hidden)) {
        item.$state.isPlaying.set(true);
      }

      promise.catch((e) => {
        if (!audio.isConnected) {
          /* The node was removed from the DOM, we can ignore this error */
          return;
        }

        // Interrupted by a subsequent load() or pause() — benign. Crucially
        // the isPlaying intent must survive so stall recovery (see
        // `waitingEvent`) can resume playback on the next canplay.
        if (e?.name === "AbortError") return;

        const err =
          "Couldn't play audio automatically. Please resume playback manually.";
        console.error(err, e);
        item.intendsToPlay = false;
        item.$state.isPlaying.set(false);
      });
    });
  }

  /**
   * Use this function to reload the audio after an error occurred.
   *
   * @type {Actions["reload"]}
   */
  reload(args) {
    this.#withAudioNode(args.audioId, (audio, item) => {
      if (audio.readyState === 0 || audio.error?.code === 2) {
        audio.load();

        if (args.progress !== undefined) {
          item.setAttribute(
            "initial-progress",
            JSON.stringify(args.progress),
          );
        }

        if (args.play) {
          this.play({ audioId: args.audioId, volume: audio.volume });
        }
      }
    });
  }

  /**
   * @type {Actions["seek"]}
   */
  seek({ audioId, currentTime, percentage }) {
    this.#withAudioNode(audioId, (audio) => {
      if (currentTime != undefined) {
        audio.currentTime = currentTime;
      } else if (
        percentage != undefined && !isNaN(audio.duration) &&
        audio.duration !== Infinity
      ) {
        audio.currentTime = percentage * audio.duration;
      }
    });
  }

  /**
   * @type {Actions["supply"]}
   */
  supply(args) {
    const existingMap = new Map(this.#items.value.map((a) => [a.id, a]));

    // Start loading new streams
    for (const item of args.audio) {
      if (
        "stream" in item &&
        !existingMap.has(item.id) &&
        !this.#streams.has(item.id)
      ) {
        this.#streams.set(item.id, item.stream);
        this.#resolveStream(
          item.id,
          item.stream,
          item.mimeType ?? "",
          item.seek,
          item.duration,
          this.#streamAbort.signal,
        );
      }
    }

    // Stop streams that are no longer needed
    const newIds = new Set(args.audio.map((a) => a.id));

    for (const [id, objectUrl] of this.#mediaSourceUrls) {
      if (!newIds.has(id)) {
        URL.revokeObjectURL(objectUrl);
        this.#mediaSourceUrls.delete(id);
      }
    }

    for (const id of this.#streams.keys()) {
      if (!newIds.has(id)) this.#streams.delete(id);
    }

    /** @type {AudioUrl[]} Remove `stream` field, replace it with `url` */
    const resolvedAudio = args.audio.map((a) => {
      let url = "stream" in a ? this.#mediaSourceUrls.get(a.id) : a.url;

      if (!url && "stream" in a && this.#streams.has(a.id)) {
        // #resolveStream creates the media source URL synchronously,
        // so this should be unreachable.
        throw new Error("Stream did not produce a media source url");
      }

      // A stream rejected by #resolveStream (e.g. MediaSource unsupported
      // on this browser) renders without a source; #resolveStream flags
      // the error on the item's state instead.
      url = url ?? "";

      return {
        id: a.id,
        isPreload: a.isPreload,
        mimeType: a.mimeType,
        progress: a.progress,
        track: a.track,
        url,
      };
    });

    const hasNewIds = resolvedAudio.some((a) => !existingMap.has(a.id));
    const hasPreloadChanges = resolvedAudio.some(
      (a) => existingMap.get(a.id)?.isPreload !== a.isPreload,
    );

    const hasUrlChanges = resolvedAudio.some(
      (a) => existingMap.get(a.id)?.url !== a.url,
    );

    if (hasNewIds || hasPreloadChanges || hasUrlChanges) {
      this.#items.value = resolvedAudio;
    }

    // When only the URL changed for an existing item (e.g. tab leadership handoff invalidated
    // a blob URL), the same <de-audio-item> element is reused via `keyed`. lit-html will
    // update <source src> but the browser won't reload on its own — call audio.load() if the
    // element hasn't successfully loaded yet so it picks up the fresh URL.
    if (hasUrlChanges && !hasNewIds) {
      for (const a of resolvedAudio) {
        if (existingMap.has(a.id) && existingMap.get(a.id)?.url !== a.url) {
          this.#withAudioNode(a.id, (audio) => {
            // Clear any `src` attribute left behind by a previously
            // stream-backed track (#resolveStream sets it imperatively):
            // it takes precedence over the <source> element and may point
            // at a revoked object URL.
            audio.removeAttribute("src");
            if (audio.readyState === 0 || audio.error) audio.load();
          });
        }
      }
    }

    if (args.play) this.play(args.play);
  }

  // STREAMS

  /**
   * @param {string} id
   * @param {ReadableStream} stream
   * @param {string} mimeType
   * @param {((timeSeconds: number) => Promise<ReadableStream>) | undefined} seekFn
   * @param {number | undefined} duration
   * @param {AbortSignal} [signal]
   */
  async #resolveStream(id, stream, mimeType, seekFn, duration, signal) {
    // MediaSource is unavailable on iPhone before iOS 17.1, so bail out
    // early when MSE (or its managed variant) is missing, or when the mime
    // type is unsupported — otherwise the item would hang in a loading
    // state forever with an unhandled rejection.
    const win = /** @type {any} */ (globalThis);
    const MediaSourceCtor = /** @type {typeof MediaSource | undefined} */ (
      win.MediaSource ?? win.ManagedMediaSource
    );

    if (
      !MediaSourceCtor || !mimeType ||
      !MediaSourceCtor.isTypeSupported(mimeType)
    ) {
      // Delete synchronously so `supply()` treats the stream as resolved
      // (it renders the item without a source), then flag the error on the
      // item's state once its element exists.
      this.#streams.delete(id);
      stream.cancel().catch(() => {});
      Promise.resolve().then(() => {
        this.#itemElement(id)?.$state.loadingState.set({
          error: { code: 4 }, // MEDIA_ERR_SRC_NOT_SUPPORTED
        });
      });
      return;
    }

    const mediaSource = new MediaSourceCtor();
    const objectUrl = URL.createObjectURL(mediaSource);

    this.#mediaSourceUrls.set(id, objectUrl);
    this.#streams.delete(id);

    // Yield so the render triggered by supply() can complete, ensuring the
    // audio element is in the DOM before we set its src.
    await Promise.resolve();

    if (!this.#mediaSourceUrls.has(id)) {
      // Item was removed while waiting
      URL.revokeObjectURL(objectUrl);
      return;
    }

    const itemEl = this.#itemElement(id);
    if (!itemEl) {
      URL.revokeObjectURL(objectUrl);
      this.#mediaSourceUrls.delete(id);
      return;
    }

    // MediaSource must be attached via audio.src directly;
    // <source> elements do not trigger sourceopen.
    itemEl.audio.src = objectUrl;

    // Wait for `sourceopen`, but bail out if the element is torn down while
    // we're still waiting (sourceopen may never fire then).
    await new Promise((resolve) => {
      const onOpen = () => {
        cleanup();
        resolve(undefined);
      };
      const onAbort = () => {
        cleanup();
        resolve(undefined);
      };
      const cleanup = () => {
        mediaSource.removeEventListener("sourceopen", onOpen);
        signal?.removeEventListener("abort", onAbort);
      };

      mediaSource.addEventListener("sourceopen", onOpen, { once: true });
      signal?.addEventListener("abort", onAbort, { once: true });
    });

    if (!this.#mediaSourceUrls.has(id)) {
      // The item was removed — or the engine torn down — while awaiting
      // `sourceopen`. Nothing to buffer; release the URL if it wasn't already
      // revoked (e.g. by supply()).
      URL.revokeObjectURL(objectUrl);
      this.#mediaSourceUrls.delete(id);
      return;
    }

    // 'reader' is always the current active reader; the seeking handler
    // closes over this variable so it always cancels the right one.
    let reader = stream.getReader();
    let seekPending = false;
    let seekTarget = 0;

    const onSeeking = () => {
      if (!seekFn) return;
      const audio = itemEl.audio;
      const target = audio.currentTime;

      // Only intervene if the target is outside what's already buffered.
      for (let i = 0; i < audio.buffered.length; i++) {
        if (
          audio.buffered.start(i) <= target && target <= audio.buffered.end(i)
        ) {
          return; // Browser can handle it with buffered data.
        }
      }

      seekPending = true;
      seekTarget = target;
      reader.cancel().catch(() => {});
    };

    itemEl.audio.addEventListener("seeking", onSeeking);

    try {
      if (duration !== undefined) mediaSource.duration = duration;

      const sourceBuffer = mediaSource.addSourceBuffer(mimeType);

      while (true) {
        if (!this.#mediaSourceUrls.has(id)) {
          await reader.cancel();
          break;
        }

        let done, value;

        try {
          ({ done, value } = await reader.read());
        } catch {
          done = true;
        }

        if (!this.#mediaSourceUrls.has(id)) break;

        if (seekPending) {
          seekPending = false;

          // Clear all buffered data before feeding from the new position.
          if (sourceBuffer.updating) await waitForUpdateEnd(sourceBuffer);
          const removal = waitForUpdateEnd(sourceBuffer);
          sourceBuffer.remove(0, Infinity);
          if (!(await removal)) {
            throw new Error("SourceBuffer remove failed");
          }

          if (!seekFn) throw new Error("seekFn is undefined");
          reader = (await seekFn(seekTarget)).getReader();

          continue;
        }

        if (done) {
          if (mediaSource.readyState === "open") mediaSource.endOfStream();
          break;
        }

        if (sourceBuffer.updating) await waitForUpdateEnd(sourceBuffer);

        const appending = waitForUpdateEnd(sourceBuffer);
        sourceBuffer.appendBuffer(value);
        if (!(await appending)) {
          throw new Error("SourceBuffer append failed");
        }
      }
    } catch (err) {
      console.error("[audio engine] Stream error:", err);
      if (mediaSource.readyState === "open") mediaSource.endOfStream("decode");

      // Only surface the error if this stream is still the item's source —
      // on iOS the node may already have been reused for another track.
      if (this.#mediaSourceUrls.get(id) === objectUrl) {
        itemEl.$state.loadingState.set({ error: { code: 3 } });
      }
    } finally {
      itemEl.audio.removeEventListener("seeking", onSeeking);
    }
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const allItems = this.items();

    // Render every item, including the preloaded next track. On iOS this is
    // what lets the next track's bytes buffer while the current one plays, so
    // the locked-screen handoff only needs play() — no background load() that
    // would tear down the audio session and leave playback silent.
    const items = allItems;

    const ids = allItems.map((i) => i.id);

    this.querySelectorAll("de-audio-item").forEach((element) => {
      if (ids.includes(element.id)) return;

      // Detached media elements can keep playing (notorious on iOS, but
      // possible elsewhere too). Updating <source src> alone doesn't stop
      // that — resource selection only re-runs on load() — so fully unload
      // the node before lit-html drops it.
      const audio = element.querySelector("audio");
      if (!audio) return;

      // Unhook it from the Web Audio graph (a `createMediaElementSource` node
      // can only be made once per element, and the element is about to be
      // dropped, so the source node must be released).
      this[UNROUTE_AUDIO](audio);

      audio.pause();
      audio.removeAttribute("src");
      audio.querySelectorAll("source").forEach((s) => s.removeAttribute("src"));
      audio.load();
    });

    const group = this.group;
    const nodes = items.map((audio) => {
      const ip = audio.progress === undefined
        ? "0"
        : JSON.stringify(audio.progress);

      return keyed(
        audio.id,
        html`
          <de-audio-item
            group="${this.broadcasted ? `${group}/${audio.id}` : nothing}"
            id="${audio.id}"
            initial-progress="${ip}"
            mime-type="${audio.mimeType ? audio.mimeType : nothing}"
            preload="${audio.isPreload ? `preload` : nothing}"
            url="${audio.url ?? nothing}"
          >
            <audio
              crossorigin="anonymous"
              preload="auto"
            >
              ${audio.url
                ? html`
                  <source
                    src="${audio.url}"
                    ${audio.mimeType ? 'type="' + audio.mimeType + '"' : ""}
                  />
                `
                : nothing}
            </audio>
          </de-audio-item>
        `,
      );
    });

    return html`
      <section id="audio-nodes">
        ${nodes}
      </section>
    `;
  }

  // 🛠️

  /**
   * Get the state of a single audio item.
   *
   * @param {string} audioId
   * @returns {SignalReader<AudioStateReadOnly | undefined>}
   */
  _state(audioId) {
    return computed(() => {
      const _trigger = this.#items.value;

      const s = this.#itemElement(audioId)?.state;
      return s ? { ...s } : undefined;
    });
  }

  /**
   * Get the state of a single audio item.
   *
   * @param {string} audioId
   * @returns {AudioStateReadOnly | undefined}
   */
  state(audioId) {
    return this._state(audioId)();
  }

  /**
   * @param {string} audioId
   */
  #itemElement(audioId) {
    const node = this.querySelector(
      `de-audio-item[id="${audioId}"]:not([preload])`,
    ) ?? this.querySelector(
      `de-audio-item[id="${audioId}"]`,
    );

    if (node) {
      const item = /** @type {AudioEngineItem} */ (node);
      return item;
    }
  }

  /**
   * @param {string} audioId
   * @param {(audio: HTMLAudioElement, item: AudioEngineItem) => void} fn
   */
  #withAudioNode(audioId, fn) {
    const item = this.#itemElement(audioId);
    if (item) fn(item.audio, item);
  }

  // WEB AUDIO

  /**
   * The shared Web Audio graph, exposed so consumers (equalizer / visualizer
   * plugins, themes, …) can hook into the audio signal.
   *
   * Every <audio> element is routed into the `input` node (with master volume
   * already applied). By default `input` is connected straight to the
   * destination:
   *
   *   source -> input -> destination
   *
   * To insert processing, disconnect that pass-through edge and reconnect it
   * through your own chain, ending at the destination. For example, wiring in
   * a biquad filter chain and an analyser:
   *
   * ```ignore
   * const { context, input, destination } = engine.webAudio;
   * const eq = context.createBiquadFilter();
   * const analyser = context.createAnalyser();
   * input.disconnect(destination);  // remove the default pass-through
   * input.connect(eq);              // re-route through your chain
   * eq.connect(analyser);
   * analyser.connect(destination);
   * ```
   *
   * Use `context.resume()` to unlock the context on a user gesture (browsers
   * start it suspended until interaction).
   */
  get webAudio() {
    this.#ensureAudioGraph();

    return {
      context: /** @type {AudioContext} */ (this.#audioContext),
      input: /** @type {GainNode} */ (this.#input),
      destination: /** @type {AudioContext} */ (this.#audioContext).destination,
    };
  }

  /** Lazily creates the shared AudioContext and the default (pass-through) graph. */
  #ensureAudioGraph() {
    if (this.#audioContext) return;

    /** @type {typeof AudioContext | undefined} */
    const Ctx = globalThis.AudioContext ?? /** @type {any} */ (globalThis)
        .webkitAudioContext;
    if (!Ctx) return;

    const context = new Ctx();
    const input = context.createGain();

    // Pass-through; consumers disconnect this edge to insert their chain.
    input.connect(context.destination);

    this.#audioContext = context;
    this.#input = input;

    // Apply the (possibly persisted) master volume to the freshly created node.
    input.gain.value = this.#volume.value;

    // Unlock the context on the first user gesture so playback that was
    // requested before any interaction (e.g. autoplay) can proceed.
    if (context.state === "suspended") {
      const unlock = () => {
        context.resume().catch(() => {});
        ["touchstart", "touchend", "mousedown", "keydown"].forEach((e) => {
          document.body.removeEventListener(e, unlock);
        });
      };
      ["touchstart", "touchend", "mousedown", "keydown"].forEach((e) => {
        document.body.addEventListener(e, unlock);
      });
    }
  }

  /**
   * Routes an <audio> element through the Web Audio graph. Safe to call more
   * than once per element (eg. when a single node is reused on iOS).
   *
   * Module-internal; use `webAudio` to consume the graph.
   *
   * @param {HTMLAudioElement} audio
   */
  [ROUTE_AUDIO](audio) {
    if (this.#sourceNodes.has(audio)) return;

    this.#ensureAudioGraph();
    if (!this.#audioContext || !this.#input) return;

    let source;
    try {
      source = this.#audioContext.createMediaElementSource(audio);
    } catch {
      // A `createMediaElementSource` node can only be created once per element.
      // Treat a reused element we don't hold a node for (e.g. after the engine
      // was torn down and reconnected) as already routed and skip it — the
      // element simply won't be part of the graph in that case.
      this.#sourceNodes.set(audio, /** @type {any} */ (null));
      return;
    }
    source.connect(this.#input);
    this.#sourceNodes.set(audio, source);

    // Volume is handled by the graph's input gain node; keep the element's own
    // volume at unity so the two don't compound.
    audio.volume = 1;
  }

  /**
   * Removes an <audio> element's source node from the graph. Mostly useful
   * when the element is dropped (no longer in `this.items()`).
   *
   * Module-internal; use `webAudio` to consume the graph.
   *
   * @param {HTMLAudioElement} audio
   */
  [UNROUTE_AUDIO](audio) {
    const source = this.#sourceNodes.get(audio);
    if (!source) return;

    source.disconnect();
    this.#sourceNodes.delete(audio);
  }

  /** Resumes the shared AudioContext if it is suspended (e.g. autoplay policy). */
  #resumeContext() {
    this.#audioContext?.resume().catch(() => {});
  }

  /** Tears down the graph and all per-element source nodes. */
  #teardownWebAudio() {
    for (const audio of this.#sourceNodes.keys()) {
      this[UNROUTE_AUDIO](audio);
    }

    this.#sourceNodes.clear();

    // Detach from the destination, then close the context to free resources.
    if (this.#input) this.#input.disconnect();
    this.#audioContext?.close().catch(() => {});

    this.#audioContext = undefined;
    this.#input = undefined;
  }
}

export default AudioEngine;

////////////////////////////////////////////
// ITEM ELEMENT
////////////////////////////////////////////

class AudioEngineItem extends BroadcastableDiffuseElement {
  static NAME = "diffuse/engine/audio/item";
  static observedAttributes = ["preload"];

  constructor() {
    super();

    // TODO:
    // const ip = this.getAttribute("initial-progress");

    /**
     * Playback was requested but hasn't (visibly) started yet. Unlike
     * `$state.isPlaying` this is never claimed optimistically on iOS while
     * hidden, so it survives the "play() resolved but nothing plays" case
     * and lets the engine resume on refocus. Cleared once playback truly
     * starts, on explicit pause, or when playback fails in the foreground.
     */
    this.intendsToPlay = false;

    /**
     * @type {AudioState}
     */
    this.$state = {
      currentTime: signal(0),
      duration: signal(0),
      hasEnded: signal(false),
      isPlaying: signal(false),
      isPreload: signal(this.hasAttribute("preload")),
      loadingState: signal(/** @type {LoadingState} */ ("initialisation")),

      progress: computed(() => {
        const currentTime = this.$state.currentTime.value;
        const duration = this.$state.duration.value;

        if (!duration || isNaN(duration) || duration === Infinity) return 0;

        return currentTime / duration;
      }),
    };
  }

  /**
   * @override
   * @param {string} name
   * @param {string} oldValue
   * @param {string} newValue
   */
  attributeChangedCallback(name, oldValue, newValue) {
    super.attributeChangedCallback(name, oldValue, newValue);
    if (name === "preload") {
      this.$state.isPreload.set(newValue !== null);
    }
  }

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    const audio = this.audio;

    audio.addEventListener("canplay", this.canplayEvent);
    audio.addEventListener("durationchange", this.durationchangeEvent);
    audio.addEventListener("ended", this.endedEvent);
    audio.addEventListener("error", this.errorEvent);
    audio.addEventListener("pause", this.pauseEvent);
    audio.addEventListener("play", this.playEvent);
    audio.addEventListener("playing", this.playingEvent);
    audio.addEventListener("suspend", this.suspendEvent);
    audio.addEventListener("timeupdate", this.timeupdateEvent);
    audio.addEventListener("waiting", this.waitingEvent);

    // Transition from initialisation to loading for non-preload items
    if (!this.hasAttribute("preload")) {
      this.$state.loadingState.set("loading");
    }

    // Setup broadcasting if part of group
    if (this.hasAttribute("group")) {
      const actions = this.broadcast(
        this.identifier,
        {
          getCurrentTime: {
            strategy: "leaderOnly",
            fn: this.$state.currentTime.get,
          },
          getDuration: { strategy: "leaderOnly", fn: this.$state.duration.get },
          getHasEnded: { strategy: "leaderOnly", fn: this.$state.hasEnded.get },
          getIsPlaying: {
            strategy: "leaderOnly",
            fn: this.$state.isPlaying.get,
          },
          getIsPreload: {
            strategy: "leaderOnly",
            fn: this.$state.isPreload.get,
          },
          getLoadingState: {
            strategy: "leaderOnly",
            fn: this.$state.loadingState.get,
          },

          // SET
          setCurrentTime: {
            strategy: "replicate",
            fn: this.$state.currentTime.set,
          },
          setDuration: { strategy: "replicate", fn: this.$state.duration.set },
          setHasEnded: { strategy: "replicate", fn: this.$state.hasEnded.set },
          setIsPlaying: {
            strategy: "replicate",
            fn: this.$state.isPlaying.set,
          },
          setIsPreload: {
            strategy: "replicate",
            fn: this.$state.isPreload.set,
          },
          setLoadingState: {
            strategy: "replicate",
            fn: this.$state.loadingState.set,
          },
        },
        {
          // Sync leadership with engine's broadcasting channel
          assumeLeadership: (await this.engine?.broadcastingStatus())?.leader,
        },
      );

      if (actions) {
        this.$state.currentTime.set = actions.setCurrentTime;
        this.$state.duration.set = actions.setDuration;
        this.$state.hasEnded.set = actions.setHasEnded;
        this.$state.isPlaying.set = actions.setIsPlaying;
        this.$state.isPreload.set = actions.setIsPreload;
        this.$state.loadingState.set = actions.setLoadingState;

        untracked(async () => {
          this.$state.currentTime.value = await actions.getCurrentTime();
          this.$state.duration.value = await actions.getDuration();
          this.$state.hasEnded.value = await actions.getHasEnded();
          this.$state.isPlaying.value = await actions.getIsPlaying();
          this.$state.isPreload.value = await actions.getIsPreload();
          this.$state.loadingState.value = await actions.getLoadingState();
        });
      }
    }

    // Super
    super.connectedCallback();

    // Route this item's <audio> through the engine's shared Web Audio graph
    // so volume flows through the gain node and consumers (equalizer,
    // visualizer plugins, etc) can tap into the signal. Idempotent per element.
    this.engine?.[ROUTE_AUDIO](this.audio);
  }

  /**
   * @override
   */
  disconnectedCallback() {
    // Unhook the source node so the engine can tear the graph down cleanly
    // once the item is dropped. The engine also handles this in its render
    // cleanup, so this is just a safety net.
    let audio;
    try {
      audio = this.audio;
    } catch {
      return;
    }
    this.engine?.[UNROUTE_AUDIO](audio);
    super.disconnectedCallback();
  }

  // STATE

  /**
   * @type {AudioStateReadOnly}
   */
  get state() {
    return {
      id: this.id,
      mimeType: this.getAttribute("mime-type") ?? undefined,
      url: this.getAttribute("url") ?? "",

      currentTime: this.$state.currentTime.get,
      duration: this.$state.duration.get,
      hasEnded: this.$state.hasEnded.get,
      isPlaying: this.$state.isPlaying.get,
      isPreload: this.$state.isPreload.get,
      loadingState: this.$state.loadingState.get,

      progress: this.$state.progress,
    };
  }

  // RELATED ELEMENTS

  get audio() {
    const el = this.querySelector("audio");
    if (el) return /** @type {HTMLAudioElement} */ (el);
    else throw new Error("Cannot find child audio element");
  }

  get engine() {
    const el = this.closest("de-audio");
    if (el) return /** @type {AudioEngine} */ (el);
    else return null;
  }

  // EVENTS

  /**
   * @param {Event} event
   */
  canplayEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const item = engineItem(audio);

    if (
      item?.hasAttribute("initial-progress") &&
      audio.duration &&
      !isNaN(audio.duration)
    ) {
      const progress = JSON.parse(
        item.getAttribute("initial-progress") ?? "0",
      );
      if (
        progress !== 0 && !isNaN(audio.duration) && audio.duration !== Infinity
      ) {
        audio.currentTime = audio.duration * progress;
      }

      item.removeAttribute("initial-progress");
    }

    finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  durationchangeEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    if (!isNaN(audio.duration)) {
      engineItem(audio)?.$state.duration.set(audio.duration);
    }
  }

  /**
   * @param {Event} event
   */
  endedEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    audio.currentTime = 0;

    const item = engineItem(audio);
    if (item) item.intendsToPlay = false;
    item?.$state.hasEnded.set(true);
  }

  /**
   * @param {Event} event
   */
  errorEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const code = audio.error?.code || 0;

    engineItem(audio)?.$state.loadingState.set({ error: { code } });
  }

  /**
   * @param {Event} event
   */
  pauseEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const item = engineItem(audio);

    item?.$state.isPlaying.set(false);
  }

  /**
   * @param {Event} event
   */
  playEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);

    const item = engineItem(audio);
    item?.$state.hasEnded.set(false);
    item?.$state.isPlaying.set(true);

    // In case audio was preloaded:
    if (audio.readyState >= 2) finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  playingEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    const item = engineItem(audio);

    // Playback truly started, intent fulfilled.
    if (item) item.intendsToPlay = false;
    item?.$state.isPlaying.set(true);

    finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  suspendEvent(event) {
    finishedLoading(event);
  }

  /**
   * @param {Event} event
   */
  timeupdateEvent(event) {
    const audio = /** @type {HTMLAudioElement} */ (event.target);
    if (isNaN(audio.duration) || audio.duration === 0) return;

    engineItem(audio)?.$state.currentTime.set(audio.currentTime);
  }

  /**
   * @param {Event} event
   */
  waitingEvent(event) {
    initiateLoading(event);

    const audio = /** @type {HTMLAudioElement} */ (event.target);
    if (audio.seeking) return;
    if (audio.networkState !== HTMLMediaElement.NETWORK_IDLE) return;

    const item = engineItem(audio);
    if (!item || item.hasAttribute("preload")) return;

    const progress = !isNaN(audio.duration) && audio.duration > 0 &&
        audio.duration !== Infinity
      ? audio.currentTime / audio.duration
      : 0;

    if (progress > 0) {
      item.setAttribute("initial-progress", JSON.stringify(progress));
    }

    // Don't force a full reload if the browser already has buffered data —
    // it should be able to continue buffering on its own. This prevents
    // discarding the preloaded buffer when playback briefly catches up to
    // the end of the downloaded portion.
    if (audio.buffered.length > 0) return;

    audio.load();

    audio.addEventListener("canplay", () => {
      if (item.$state.isPlaying.get() || item.intendsToPlay) {
        item.engine?.play({ audioId: item.id });
      }
    }, { once: true });
  }
}

export { AudioEngineItem };

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {HTMLAudioElement} audio
 */
function engineItem(audio) {
  const c = audio.closest("de-audio-item");
  if (c) return /** @type {AudioEngineItem} */ (c);
  else return null;
}

/**
 * @param {Event} event
 */
function finishedLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  engineItem(audio)?.$state.loadingState.set("loaded");
}

/**
 * @param {Event} event
 */
function initiateLoading(event) {
  const audio = /** @type {HTMLAudioElement} */ (event.target);
  if (audio.readyState < 4) {
    const item = engineItem(audio);
    if (item?.hasAttribute("preload")) return;
    item?.$state.loadingState.set("loading");
  }
}

/**
 * Resolves once the SourceBuffer finishes its current append/remove
 * operation. `true` on `updateend`, `false` on `updateerror` (which
 * WebKit's MSE may fire without a following `updateend`).
 *
 * @param {SourceBuffer} sourceBuffer
 * @returns {Promise<boolean>}
 */
function waitForUpdateEnd(sourceBuffer) {
  return new Promise((resolve) => {
    const onEnd = () => {
      cleanup();
      resolve(true);
    };
    const onError = () => {
      cleanup();
      resolve(false);
    };
    const cleanup = () => {
      sourceBuffer.removeEventListener("updateend", onEnd);
      sourceBuffer.removeEventListener("updateerror", onError);
    };

    sourceBuffer.addEventListener("updateend", onEnd);
    sourceBuffer.addEventListener("updateerror", onError);
  });
}

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = AudioEngine;
export const NAME = "de-audio";
export const NAME_ITEM = "de-audio-item";

defineElement(NAME, AudioEngine);
defineElement(NAME_ITEM, AudioEngineItem);
