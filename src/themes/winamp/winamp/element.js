import {
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";
import { computed, signal, untracked } from "~/common/signal.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 *
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import AudioEngine from "~/components/engine/audio/element.js"
 * @import QueueEngine from "~/components/engine/queue/element.js"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
 */

////////////////////////////////////////////
// UI STATE
////////////////////////////////////////////

const UI_STATE_KEY = "themes/winamp/winamp/ui";

/** @returns {{ eqOpen: boolean, playlistOpen: boolean, eqOn: boolean, eqSliders: Record<string, number> | null }} */
function loadUiState() {
  try {
    return {
      eqOpen: true,
      playlistOpen: true,
      eqOn: false,
      eqSliders: null,
      ...JSON.parse(localStorage.getItem(UI_STATE_KEY) ?? "{}"),
    };
  } catch {
    return { eqOpen: true, playlistOpen: true, eqOn: false, eqSliders: null };
  }
}

////////////////////////////////////////////
// EQ
////////////////////////////////////////////

const EQ_BANDS = [60, 170, 310, 600, 1000, 3000, 6000, 12000, 14000, 16000];

// EQ band at center (0dB): spriteNumber(50) = 14, spriteOffsets(14) = {x:0, y:1}
// backgroundPosition = "0px -65px", handle top = (62-11) * (1 - 0.5) = 26px

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

class WinampElement extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS - UI

  #marqueeOverride = signal(/** @type {string | null} */ (null));
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #marqueeOverrideTimeout = undefined;
  #marqueeCurrentOffset = 0;
  /** @type {HTMLElement | null} */
  #marqueeScroller = null;
  /** @type {ReturnType<typeof setInterval> | undefined} */
  #marqueeStepInterval = undefined;
  #marqueeText = signal("");
  #selectedTrackId = signal(/** @type {string | null} */ (null));
  #eqOpen = signal(true);
  #playlistOpen = signal(true);

  // SIGNALS - DEPENDENCIES

  $audio = signal(/** @type {AudioEngine | undefined} */ (undefined));
  $output = signal(/** @type {OutputElement | undefined} */ (undefined));
  $queue = signal(/** @type {QueueEngine | undefined} */ (undefined));
  $repeatShuffle = signal(
    /** @type {RepeatShuffleEngine | undefined} */ (undefined),
  );

  // SIGNALS - COMPUTED

  audio = computed(() => {
    const curr = this.$queue.value?.now();
    return curr ? this.$audio.value?.state(curr.id) : undefined;
  });

  currentTrack = computed(() => {
    const item = this.$queue.value?.now();
    if (!item) return undefined;
    const col = this.$output.value?.tracks.collection();
    if (!col || col.state !== "loaded") return undefined;
    return col.data.find((t) => t.id === item.id);
  });

  isPlaying = computed(() => {
    return this.$audio.value?.isPlaying();
  });

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {AudioEngine} */
    const audio = query(this, "audio-engine-selector");

    /** @type {OutputElement} */
    const output = query(this, "output-selector");

    /** @type {QueueEngine} */
    const queue = query(this, "queue-engine-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    whenElementsDefined({ audio, output, queue, repeatShuffle }).then(() => {
      this.$audio.value = audio;
      this.$output.value = output;
      this.$queue.value = queue;
      this.$repeatShuffle.value = repeatShuffle;

      this.effect(() => {
        const track = this.currentTrack();
        if (!track) return; // preserve last text during track transitions
        const aud = this.audio();

        const artist = (track.tags?.artist ?? "").toUpperCase();
        const title = (track.tags?.title ?? "").toUpperCase();
        const durSeconds = track.stats?.duration
          ? track.stats.duration / 1000
          : untracked(() => aud?.duration() ?? 0);
        const durMinutes = Math.floor(durSeconds / 60);
        const durSecs = Math.floor(durSeconds % 60);
        this.#marqueeText.value = `${artist} - ${title} (${durMinutes}:${
          durSecs.toString().padStart(2, "0")
        })`;
        this.#marqueeCurrentOffset = 0;
      });
    });

    // UI State
    const ui = loadUiState();
    this.#eqOpen.value = ui.eqOpen;
    this.#playlistOpen.value = ui.playlistOpen;

    this.forceRender();
    this.#marqueeScroller = this.root().querySelector("#marquee > div");
    requestAnimationFrame(() => this.#drawEqGraph());

    this.#marqueeStepInterval = setInterval(() => {
      const text = untracked(
        () => this.#marqueeOverride.value ?? this.#marqueeText.value,
      );
      const MAX = WinampElement.#MARQUEE_MAX_LENGTH;
      const W = WinampElement.#CHAR_WIDTH;
      const SEP = WinampElement.#MARQUEE_SEPARATOR;
      if (text.length >= MAX) {
        const stringLength = (text.length + SEP.length) * W;
        this.#marqueeCurrentOffset = WinampElement.#marqueeMod(
          this.#marqueeCurrentOffset + W,
          stringLength,
        );
      } else {
        this.#marqueeCurrentOffset = 0;
      }
      if (!this.#marqueeScroller?.isConnected) {
        this.#marqueeScroller = this.root().querySelector("#marquee > div");
      }
      if (this.#marqueeScroller) {
        this.#marqueeScroller.style.transform =
          `translateX(-${this.#marqueeCurrentOffset}px)`;
      }
    }, 220);

    // winamp-active press feedback via event delegation
    this.root().addEventListener("pointerdown", (e) => {
      if (!(e.target instanceof HTMLElement)) return;
      if (e.target.tagName !== "DIV" || !e.target.id) return;
      const el = e.target;
      el.classList.add("winamp-active");
      const cleanup = () => {
        el.classList.remove("winamp-active");
        document.removeEventListener("pointerup", cleanup);
      };
      document.addEventListener("pointerup", cleanup);
    });
  }

  disconnectedCallback() {
    clearInterval(this.#marqueeStepInterval);
  }

  // MARQUEE

  static #MARQUEE_MAX_LENGTH = 31;
  static #MARQUEE_SEPARATOR = "  ***  ";
  static #CHAR_WIDTH = 5;

  static #marqueeMod(n, m) {
    return ((n % m) + m) % m;
  }

  static #marqueeLoopText(text) {
    const MAX = WinampElement.#MARQUEE_MAX_LENGTH;
    return text.length >= MAX
      ? `${text}${WinampElement.#MARQUEE_SEPARATOR}${text}`
      : text.padEnd(MAX, " ");
  }

  // EQ GRAPH

  // EQ_GRAPH_LINE_COLORS: 1×19px gradient from webamp's default skin
  static #EQ_COLORS = [
    [211, 34, 27],
    [239, 82, 33],
    [239, 123, 33],
    [224, 146, 40],
    [224, 146, 40],
    [224, 146, 40],
    [224, 178, 40],
    [239, 220, 49],
    [239, 220, 49],
    [239, 220, 49],
    [210, 235, 53],
    [210, 235, 53],
    [164, 226, 56],
    [164, 226, 56],
    [137, 226, 48],
    [113, 205, 52],
    [90, 176, 44],
    [42, 154, 22],
    [42, 154, 22],
  ];

  #drawEqGraph() {
    const canvas = this.root().querySelector("#eqGraph");
    if (!(canvas instanceof HTMLCanvasElement)) return;
    const ctx = canvas.getContext("2d");
    if (!ctx) return;

    // All bands at center (50 = 0dB): y = round(0.5 * (19-1)) = 9
    const y = 9;
    const paddingLeft = 2;
    const maxX = 108; // 9 intervals × 12px for 10 bands

    for (let x = 0; x <= maxX; x++) {
      const [r, g, b] = WinampElement.#EQ_COLORS[y];
      ctx.fillStyle = `rgb(${r},${g},${b})`;
      ctx.fillRect(paddingLeft + x, y, 1, 1);
    }
  }

  // EVENTS

  /** @param {Event} e */
  #onVolumeInput = (e) => {
    if (!(e.target instanceof HTMLInputElement)) return;
    const volume = Number(e.target.value) / 100;
    this.$audio.value?.adjustVolume({ volume });

    this.#marqueeOverride.value = `Volume: ${Math.round(volume * 100)}%`;
    clearTimeout(this.#marqueeOverrideTimeout);
    this.#marqueeOverrideTimeout = setTimeout(() => {
      this.#marqueeOverride.value = null;
    }, 2000);
  };

  /** @param {Event} e */
  #onPositionInput = (e) => {
    if (!(e.target instanceof HTMLInputElement)) return;
    const percentage = Number(e.target.value) / 100;
    const audioId = this.$queue.value?.now()?.id;
    if (audioId) this.$audio.value?.seek({ audioId, percentage });
  };

  #playPause = () => {
    const audioId = this.$queue.value?.now()?.id;
    if (this.isPlaying() && audioId) {
      this.$audio.value?.pause({ audioId });
    } else if (audioId) {
      this.$audio.value?.play({ audioId });
    }
  };

  #next = () => {
    this.$queue.value?.shift();
  };

  #previous = () => {
    this.$queue.value?.unshift();
  };

  #toggleShuffle = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setShuffle(!rs.shuffle());
  };

  #toggleRepeat = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setRepeat(!rs.repeat());
  };

  #toggleEq = () => {
    this.#eqOpen.value = !this.#eqOpen.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, eqOpen: this.#eqOpen.value }),
    );
  };

  #togglePlaylist = () => {
    this.#playlistOpen.value = !this.#playlistOpen.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, playlistOpen: this.#playlistOpen.value }),
    );
  };

  /** @param {string} id */
  #selectTrack = (id) => {
    this.#selectedTrackId.value = id;
  };

  /** @param {string} id */
  #playTrack = (id) => {
    this.#selectedTrackId.value = id;
    const queue = this.$queue.value;
    if (!queue) return;
    const past = queue.past();
    const now = queue.now();
    const future = queue.future();
    if (now?.id === id) return;
    const pastIdx = past.findIndex((i) => i.id === id);
    if (pastIdx !== -1) {
      const stepsBack = past.length - pastIdx;
      for (let i = 0; i < stepsBack; i++) queue.unshift();
      return;
    }
    const futureIdx = future.findIndex((i) => i.id === id);
    if (futureIdx !== -1) {
      const stepsForward = futureIdx + 1;
      for (let i = 0; i < stepsForward; i++) queue.shift();
    }
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const bands = EQ_BANDS;

    const volume = this.$audio.value?.volume() ?? 1;
    const volumeSprite = Math.round(volume * 28);
    const volumeBgPos = `0 -${(volumeSprite - 1) * 15}px`;

    const audio = this.audio();

    const timeSeconds = audio?.currentTime() ?? 0;
    const timeMinutes = Math.floor(timeSeconds / 60);
    const timeSecs = Math.floor(timeSeconds % 60);
    const d = {
      mFirst: Math.floor(timeMinutes / 10),
      mSecond: timeMinutes % 10,
      sFirst: Math.floor(timeSecs / 10),
      sSecond: timeSecs % 10,
    };

    // Playlist
    const queueEl = this.$queue.value;
    const nowItem = queueEl?.now();
    const allItems = [
      ...(queueEl?.past() ?? []),
      ...(nowItem ? [nowItem] : []),
      ...(queueEl?.future() ?? []),
    ];
    const col = this.$output.value?.tracks.collection();
    const trackMap = col?.state === "loaded"
      ? new Map(col.data.map((t) => [t.id, t]))
      : new Map();
    const selectedId = this.#selectedTrackId.value;
    const playlistRows = allItems.map((item, i) => {
      const track = trackMap.get(item.id);
      const isCurrent = nowItem?.id === item.id;
      const isSelected = selectedId === item.id;
      const artist = track?.tags?.artist ?? "";
      const title = track?.tags?.title ?? "";
      const label = artist ? `${artist} - ${title}` : title;
      const durSec = track?.stats?.duration ? track.stats.duration / 1000 : 0;
      const dur = durSec > 0
        ? `${Math.floor(durSec / 60)}:${String(Math.floor(durSec % 60)).padStart(2, "0")}`
        : "";
      const color = isCurrent ? "#FFFFFF" : "#00FF00";
      const bg = isSelected && !isCurrent ? "#0000FF" : "transparent";
      return { id: item.id, n: i + 1, label, dur, color, bg };
    });

    const activeMarquee = this.#marqueeOverride.value ??
      this.#marqueeText.value;
    const loopedMarquee = WinampElement.#marqueeLoopText(activeMarquee);

    const marqueeChars = [...loopedMarquee].map((char) =>
      html`
        <span class="character character-${char.toLowerCase().charCodeAt(
          0,
        )}">${char}</span>
      `
    );

    const band = html`
      <div
        class="band"
        style="background-position: 0px -65px; width: 14px; height: 63px; position: relative;"
      >
        <div
          class="slider-handle"
          style="position: absolute; top: 26px; width: 11px; height: 11px; margin-left: 1px;"
        >
        </div>
      </div>
    `;

    return html`
      <style>
      @import "./themes/winamp/vendor/webamp.css";
      </style>

      <div id="webamp">
        <div
          id="main-window"
          class="window ${this.isPlaying()
            ? "play"
            : audio
            ? "pause"
            : "stop"} draggable"
          style="position: absolute; top: 0; left: 0;"
        >
          <div id="title-bar" class="selected draggable">
            <div id="option-context"><div id="option"></div></div>
            <div id="minimize"></div>
            <div id="shade"></div>
            <div id="close"></div>
          </div>
          <div class="webamp-status">
            <div id="clutter-bar">
              <div id="button-o"></div>
              <div id="button-a"></div>
              <div id="button-i"></div>
              <div id="button-d"></div>
              <div id="button-v"></div>
            </div>
            <div id="play-pause"></div>
            <div id="work-indicator"></div>
            <div id="time">
              <div id="minus-sign"></div>
              <div id="minute-first-digit" class="digit digit-${d
                .mFirst}"></div>
              <div id="minute-second-digit" class="digit digit-${d
                .mSecond}"></div>
              <div id="second-first-digit" class="digit digit-${d
                .sFirst}"></div>
              <div id="second-second-digit" class="digit digit-${d
                .sSecond}"></div>
            </div>
          </div>
          <canvas id="visualizer" width="76" height="16"></canvas>
          <div class="media-info">
            <div id="marquee">
              <div style="white-space: nowrap; will-change: transform; font-size: 0;">
                ${marqueeChars}
              </div>
            </div>
            <div id="kbps"></div>
            <div id="khz"></div>
            <div class="mono-stereo">
              <div id="mono"></div>
              <div id="stereo"></div>
            </div>
          </div>
          <div id="volume" style="background-position: ${volumeBgPos};">
            <input
              type="range"
              min="0"
              max="100"
              value="${volume * 100}"
              @input="${this.#onVolumeInput}"
            >
          </div>
          <input type="range" id="balance" min="-100" max="100" value="0">
          <div class="windows">
            <div id="equalizer-button" class="${this.#eqOpen.value
              ? "selected"
              : ""}" @click="${this.#toggleEq}"></div>
            <div id="playlist-button" class="${this.#playlistOpen.value
              ? "selected"
              : ""}" @click="${this.#togglePlaylist}"></div>
          </div>
          <input
            type="range"
            id="position"
            min="0"
            max="100"
            .value="${(this
                .audio()?.loadingState() === "loaded"
              ? (this.audio()?.progress() ?? 0)
              : 0) * 100}"
            @input="${this.#onPositionInput}"
          >
          <div class="actions">
            <div id="previous" @click="${this.#previous}"></div>
            <div id="play" @click="${this.#playPause}"></div>
            <div id="pause" @click="${this.#playPause}"></div>
            <div id="stop"></div>
            <div id="next" @click="${this.#next}"></div>
          </div>
          <div id="eject"></div>
          <div class="shuffle-repeat">
            <div id="shuffle" class="${this.$repeatShuffle.value?.shuffle()
              ? "selected"
              : ""}" @click="${this.#toggleShuffle}"></div>
            <div id="repeat" class="${this.$repeatShuffle.value?.repeat()
              ? "selected"
              : ""}" @click="${this.#toggleRepeat}"></div>
          </div>
          <a id="about" title="About" target="_blank"></a>
        </div>

        <div
          id="equalizer-window"
          class="window draggable"
          style="position: absolute; top: 116px; left: 0; display: ${this
              .#eqOpen.value
            ? "block"
            : "none"};"
        >
          <div class="equalizer-top title-bar draggable">
            <div id="equalizer-shade"></div>
            <div id="equalizer-close"></div>
          </div>
          <input type="range" id="equalizer-volume" min="0" max="100" value="100">
          <input type="range" id="equalizer-balance" min="-100" max="100" value="0">
          <div id="on"></div>
          <div id="auto"></div>
          <canvas id="eqGraph" width="113" height="19"></canvas>
          <div id="presets-context"><div id="presets"></div></div>
          <div id="preamp">${band}</div>
          <div id="preamp-line"></div>
          <div id="plus12db"></div>
          <div id="zerodb"></div>
          <div id="minus12db"></div>
          ${bands.map((hz) =>
            html`
              <div id="band-${hz}">${band}</div>
            `
          )}
        </div>

        <div
          id="playlist-window"
          class="window draggable"
          style="position: absolute; top: 232px; left: 0; height: 116px; width: 275px; display: ${this
              .#playlistOpen.value
            ? "block"
            : "none"};"
        >
          <div class="playlist-top draggable">
            <div class="playlist-top-left draggable"></div>
            <div class="playlist-top-left-fill draggable"></div>
            <div class="playlist-top-title draggable"></div>
            <div class="playlist-top-right-fill draggable"></div>
            <div class="playlist-top-right draggable">
              <div id="playlist-shade-button"></div>
              <div id="playlist-close-button"></div>
            </div>
          </div>
          <div class="playlist-middle draggable">
            <div class="playlist-middle-left draggable"></div>
            <div class="playlist-middle-center" style="background-color: #000000; overflow-y: auto;">
              <div class="playlist-tracks">
                <div class="playlist-track-titles">
                  ${playlistRows.map((r) =>
                    html`<div class="track-cell" style="color: ${r.color}; background-color: ${r.bg};" @click="${() => this.#selectTrack(r.id)}" @dblclick="${() => this.#playTrack(r.id)}">${r.n}. ${r.label}</div>`
                  )}
                </div>
                <div class="playlist-track-durations">
                  ${playlistRows.map((r) =>
                    html`<div class="track-cell" style="color: ${r.color}; background-color: ${r.bg};" @click="${() => this.#selectTrack(r.id)}" @dblclick="${() => this.#playTrack(r.id)}">${r.dur}</div>`
                  )}
                </div>
              </div>
            </div>
            <div class="playlist-middle-right draggable">
              <div id="playlist-scroll-up-button"></div>
              <div id="playlist-scroll-down-button"></div>
            </div>
          </div>
          <div class="playlist-bottom draggable">
            <div class="playlist-bottom-left draggable">
              <div id="playlist-add-menu" class="playlist-menu"></div>
              <div id="playlist-remove-menu" class="playlist-menu"></div>
              <div id="playlist-selection-menu" class="playlist-menu"></div>
              <div id="playlist-misc-menu" class="playlist-menu"></div>
            </div>
            <div class="playlist-bottom-center draggable"></div>
            <div class="playlist-bottom-right draggable">
              <div id="playlist-list-menu" class="playlist-menu"></div>
              <div id="playlist-resize-target"></div>
            </div>
          </div>
        </div>
      </div>
    `;
  }
}

export default WinampElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WinampElement;
export const NAME = "dtw-winamp";

customElements.define(NAME, WinampElement);
