import { BroadcastableDiffuseElement, defineElement, query } from "~/common/element.js";
import { untracked } from "~/common/signal.js";

/**
 * @import {InputElement} from "@specs/components/input/types.d.ts"
 * @import {OutputElement} from "@specs/components/output/types.d.ts"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
 */

////////////////////////////////////////////
// EXPIRY
////////////////////////////////////////////

/**
 * How long before a resolved URL expires the active item is re-resolved.
 *
 * Refreshing is cheap for the inputs that hand out links, so the window is
 * generous on purpose: background tabs throttle timers, and reloading a few
 * minutes early is invisible, while a link that lapses mid-playback only
 * surfaces as a media error after the fact.
 */
export const URL_REFRESH_SKEW_SECONDS = 5 * 60;

/**
 * Retry cadence when a refresh didn't produce a usable URL (e.g. the input is
 * unreachable). Prevents a tight resolve loop off a zero-delay expiry.
 */
const REFRESH_RETRY_MS = 15_000;

/** `setTimeout` clamps delays above this to zero. */
const MAX_TIMEOUT_MS = 2 ** 31 - 1;

/**
 * Whether a resolved URL is inside its refresh window, or already expired.
 * Object URLs (and anything without a finite expiry) never need refreshing.
 *
 * @param {number | undefined} expiresAt - Unix seconds.
 * @param {number} [now] - Current time in ms; injectable for tests.
 */
export function isExpiring(expiresAt, now = Date.now()) {
  if (expiresAt === undefined || !Number.isFinite(expiresAt)) return false;
  return expiresAt - URL_REFRESH_SKEW_SECONDS <= now / 1000;
}

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * When the active queue item changes,
 * coordinate the audio engine accordingly.
 *
 * Vice versa, when the audio ends,
 * shift the queue if needed.
 */
class QueueAudioOrchestrator extends BroadcastableDiffuseElement {
  static NAME = "diffuse/orchestrator/queue-audio";

  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #refreshTimer = undefined;

  /**
   * Audio ids with a refresh in flight, so the timer and the error watcher
   * don't race for the same item.
   *
   * @type {Set<string>}
   */
  #refreshing = new Set();

  // LIFE CYCLE

  /**
   * @override
   */
  async connectedCallback() {
    // Broadcast if needed
    if (this.hasAttribute("group")) {
      this.broadcast(this.identifier, {});
    }

    // Super
    super.connectedCallback();

    /** @type {import("~/components/engine/audio/element.js").CLASS} */
    this.audio = query(this, "audio-engine-selector");

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement} */
    this.output = query(this, "output-selector");

    /** @type {import("~/components/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");

    /** @type {RepeatShuffleEngine} */
    this.repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    // Wait until defined
    await customElements.whenDefined(this.audio.localName);
    await customElements.whenDefined(this.input.localName);
    await customElements.whenDefined(this.queue.localName);
    await customElements.whenDefined(this.repeatShuffle.localName);

    // Effects
    this.effect(() => this.monitorActiveQueueItem());
    this.effect(() => this.monitorAudioEnd());
    this.effect(() => this.monitorExpiringUrl());
  }

  /**
   * @override
   */
  disconnectedCallback() {
    clearTimeout(this.#refreshTimer);
    this.#refreshTimer = undefined;

    super.disconnectedCallback();
  }

  // 🛠️

  /**
   * @param {boolean} [refresh] - Ignore an already-supplied URL (it is
   *   expiring) and reload the element on the freshly resolved one.
   */
  async monitorActiveQueueItem(refresh = false) {
    const audio = this.audio;
    const input = this.input;
    const queue = this.queue;

    if (!audio) return;
    if (!input) return;
    if (!queue) return;

    const activeItem = queue.now();
    const tracksCol = this.output?.tracks.collection();
    const tracks = tracksCol?.state === "loaded" ? tracksCol.data : undefined;

    // Read synchronously so leadership changes (e.g. tab takeover) re-trigger this effect.
    const statusPromise = this.broadcasted ? this.broadcastingStatus() : undefined;

    const activeTrack = activeItem
      ? tracks?.find((t) => t.id === activeItem.id)
      : undefined;

    const status = statusPromise ? await statusPromise : undefined;
    if (status && !status.leader) return;

    const isPlaying = untracked(audio.isPlaying);

    // Fast path: the active track may already be resolved & supplied
    // (e.g. the preload prelude resolved the next track ahead of time).
    // Reusing that URL avoids an async resolve roundtrip between the
    // `ended` event and the play() call — on iOS that gap deactivates the
    // background audio session, and the next track then silently fails to
    // start until the page is refocused. Blob URLs are excluded: they're
    // bound to the context that created them, so they always need a fresh
    // resolve (e.g. after a tab leadership handoff). So is a URL inside its
    // expiry window: its resource is (about to be) gone.
    const supplied = activeTrack
      ? untracked(() => audio.items()).find(
        (i) =>
          i.id === activeTrack.id && i.url && !i.url.startsWith("blob:") &&
          !isExpiring(i.expiresAt),
      )
      : undefined;

    // Resolve active URI
    const resolvedUri = activeTrack
      ? (supplied
        ? {
          url: supplied.url,
          mimeType: supplied.mimeType,
          expiresAt: supplied.expiresAt,
        }
        : await input.resolve({ method: "GET", uri: activeTrack.uri }))
      : undefined;

    // Check if we still need to render
    if (queue.now?.()?.id !== activeItem?.id) return;

    // A failed refresh must not drop the item that is (still) playing — that
    // is what supplying an empty list would do. Keep it and retry on the
    // refresh timer instead.
    if (refresh && !resolvedUri) {
      this.#scheduleRefresh(activeTrack?.id);
      return;
    }

    // Supply active track immediately
    // TODO: Add support for seeking streams
    //       (requires a lot of code, decoding audio frames, etc.)
    const activeAudio = activeTrack && resolvedUri
      ? [{
        id: activeTrack.id,
        isPreload: false,
        track: activeTrack,
        ...resolvedUri,
      }]
      : [];

    // A refresh replaces the source of an item that is already loaded, so the
    // position and play intent it had must survive the reload below.
    const before = refresh && activeTrack
      ? audio.state(activeTrack.id)
      : undefined;

    audio.supply({
      audio: activeAudio,
      // The reload below resumes playback; don't also restart it here.
      play: !refresh && activeItem && isPlaying
        ? { audioId: activeItem.id }
        : undefined,
    });

    if (refresh && activeTrack && resolvedUri && "url" in resolvedUri) {
      const duration = before?.duration() ?? 0;
      const progress = duration > 0 && Number.isFinite(duration)
        ? (before?.currentTime() ?? 0) / duration
        : undefined;

      // `supply` never interrupts a healthy element, so without this the new
      // URL would only take effect at the next full load.
      audio.reload({
        audioId: activeTrack.id,
        force: true,
        progress,
        play: (before?.isPlaying() ?? false) && before?.hasEnded() !== true,
      });
    }

    this.#scheduleRefresh(activeTrack?.id);
  }

  /**
   * Watch the active item for a media error while its URL is expired (or is
   * about to be). The engine retries the *same* URL, so an expired link would
   * retry up to its ceiling and never recover on its own — re-resolving is the
   * only way out.
   */
  monitorExpiringUrl() {
    const audio = this.audio;
    if (!audio) return;

    const active = audio.items().find((item) => !item.isPreload);
    if (!active) return;
    if (!isExpiring(active.expiresAt)) return;

    const loading = audio.state(active.id)?.loadingState();
    const failed = typeof loading === "object" && "error" in loading;
    if (!failed) return;

    // The effect must not track whatever the refresh reads.
    untracked(() => this.#refresh(active.id));
  }

  async monitorAudioEnd() {
    if (!this.audio) return;
    if (!this.queue) return;

    const now = this.queue.now();
    const aud = now ? this.audio.state(now.id) : undefined;

    if (aud?.hasEnded() && (await this.isLeader())) {
      if (this.repeatShuffle?.repeat() && now) {
        this.audio.seek({ audioId: now.id, currentTime: 0 });
        this.audio.play({ audioId: now.id });
        return;
      }

      await this.queue.shift();
    }
  }

  // REFRESH

  /**
   * Re-resolve the active item's URL and reload the element on it. Single
   * flight per item: the expiry timer and the error watcher can fire for the
   * same item at nearly the same time.
   *
   * @param {string} id
   */
  async #refresh(id) {
    if (this.#refreshing.has(id)) return;
    if (this.queue?.now()?.id !== id) return;

    this.#refreshing.add(id);
    try {
      await this.monitorActiveQueueItem(true);
    } finally {
      this.#refreshing.delete(id);
    }
  }

  /**
   * Arm the timer that re-resolves `id` shortly before its URL expires.
   * Items without a finite expiry (object URLs, streams) disarm it.
   *
   * @param {string | undefined} id
   */
  #scheduleRefresh(id) {
    clearTimeout(this.#refreshTimer);
    this.#refreshTimer = undefined;

    if (!id) return;

    const item = untracked(() => this.audio?.items() ?? []).find((i) =>
      i.id === id
    );
    const expiresAt = item?.expiresAt;

    if (expiresAt === undefined || !Number.isFinite(expiresAt)) return;

    // Floor the delay: a URL that is already inside the window (a previous
    // refresh failed, or an input handed out a nearly-expired link) must not
    // turn into a tight resolve loop.
    const delay = Math.max(
      REFRESH_RETRY_MS,
      (expiresAt - URL_REFRESH_SKEW_SECONDS) * 1000 - Date.now(),
    );

    this.#refreshTimer = setTimeout(() => {
      this.#refreshTimer = undefined;
      if (this.queue?.now()?.id !== id) return;
      this.#refresh(id);
    }, Math.min(delay, MAX_TIMEOUT_MS));
  }
}

export default QueueAudioOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = QueueAudioOrchestrator;
export const NAME = "do-queue-audio";

defineElement(NAME, QueueAudioOrchestrator);
