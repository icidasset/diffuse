import {
  defineElement,
  DiffuseElement,
  query,
  whenElementsDefined,
} from "~/common/element.js";
import { batch, signal, untracked } from "~/common/signal.js";
import { repeat } from "lit-html/directives/repeat.js";
import { guard } from "lit-html/directives/guard.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 *
 * @import ControllerOrchestrator from "~/components/orchestrator/controller/element.js"
 * @import RepeatShuffleEngine from "~/components/engine/repeat-shuffle/element.js"
 * @import {Item} from "~/components/engine/queue/types.d.ts"
 */

////////////////////////////////////////////
// UI STATE
////////////////////////////////////////////

const UI_STATE_KEY = "facets/themes/winamp/winamp/ui";

/** @returns {{ eqOpen: boolean, playlistOpen: boolean, milkdropOpen: boolean, eqOn: boolean, eqSliders: Record<string, number> | null, mainShade: boolean, eqShade: boolean, playlistShade: boolean, positions: Record<string, {x:number,y:number}> | null, sizes: Record<string, {width:number,height:number}> | null }} */
function loadUiState() {
  try {
    return {
      eqOpen: true,
      playlistOpen: true,
      milkdropOpen: true,
      eqOn: false,
      eqSliders: null,
      mainShade: false,
      eqShade: false,
      playlistShade: false,
      positions: null,
      sizes: null,
      ...JSON.parse(localStorage.getItem(UI_STATE_KEY) ?? "{}"),
    };
  } catch {
    return {
      eqOpen: true,
      playlistOpen: true,
      milkdropOpen: true,
      eqOn: false,
      eqSliders: null,
      mainShade: false,
      eqShade: false,
      playlistShade: false,
      positions: null,
      sizes: null,
    };
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

/**
 * Webamp's Nullsoft FFT — ported from webamp.bundle.js
 * Converts 1024 time-domain samples → 512 spectral magnitude values.
 */
class WinampFFT {
  static #TWO_PI = 6.2831853;
  static #HALF_PI = 1.5707963268;

  constructor() {
    const NFREQ = 1024; // samplesOut * 2
    this.#bitrev = this.#initBitRev(NFREQ);
    this.#cossint = this.#initCosSin(NFREQ);
    this.#envelope = this.#initEnvelope(1024, 1.0);
    this.#equalize = this.#initEqualize(NFREQ);
    this.#temp1 = new Float32Array(NFREQ);
    this.#temp2 = new Float32Array(NFREQ);
  }

  /** @type {number[]} */ #bitrev;
  /** @type {Float32Array[]} */ #cossint;
  /** @type {Float32Array} */ #envelope;
  /** @type {Float32Array} */ #equalize;
  /** @type {Float32Array} */ #temp1;
  /** @type {Float32Array} */ #temp2;

  /** @param {number} NFREQ */
  #initBitRev(NFREQ) {
    const t = Array.from({ length: NFREQ }, (_, i) => i);
    for (let i = 0, j = 0; i < NFREQ; i++) {
      if (j > i) {
        const tmp = t[i];
        t[i] = t[j];
        t[j] = tmp;
      }
      let m = NFREQ >> 1;
      while (m >= 1 && j >= m) {
        j -= m;
        m >>= 1;
      }
      j += m;
    }
    return t;
  }

  /** @param {number} NFREQ */
  #initCosSin(NFREQ) {
    const table = [];
    let d = 2;
    while (d <= NFREQ) {
      const theta = (-2.0 * Math.PI) / d;
      table.push(new Float32Array([Math.cos(theta), Math.sin(theta)]));
      d <<= 1;
    }
    return table;
  }

  /** @param {number} n @param {number} power */
  #initEnvelope(n, power) {
    const mult = (1.0 / n) * WinampFFT.#TWO_PI;
    return Float32Array.from(
      { length: n },
      (_, i) =>
        Math.pow(0.5 + 0.5 * Math.sin(i * mult - WinampFFT.#HALF_PI), power),
    );
  }

  /** @param {number} NFREQ */
  #initEqualize(NFREQ) {
    const eq = new Float32Array(NFREQ / 2);
    let bias = 0.04;
    for (let i = 0; i < NFREQ / 2; i++) {
      const inv = (9.0 - bias) / (NFREQ / 2);
      eq[i] = Math.log10(1.0 + bias + (i + 1) * inv);
      bias /= 1.0025;
    }
    return eq;
  }

  /**
   * @param {Float32Array} inWave 1024 samples
   * @param {Float32Array} outSpec 512 output magnitudes
   */
  timeToFrequencyDomain(inWave, outSpec) {
    const real = this.#temp1;
    const imag = this.#temp2;
    const bitrev = this.#bitrev;
    const env = this.#envelope;

    for (let i = 0; i < real.length; i++) {
      const idx = bitrev[i];
      real[i] = idx < inWave.length ? inWave[idx] * env[idx] : 0;
    }
    imag.fill(0);

    let dftsize = 2, t = 0;
    while (dftsize <= real.length) {
      const [wpr, wpi] = this.#cossint[t];
      let wr = 1.0, wi = 0.0;
      const half = dftsize >> 1;
      for (let m = 0; m < half; m++) {
        for (let i = m; i < real.length; i += dftsize) {
          const j = i + half;
          const tr = wr * real[j] - wi * imag[j];
          const ti = wr * imag[j] + wi * real[j];
          real[j] = real[i] - tr;
          imag[j] = imag[i] - ti;
          real[i] += tr;
          imag[i] += ti;
        }
        const wt = wr;
        wr = wr * wpr - wi * wpi;
        wi = wi * wpr + wt * wpi;
      }
      dftsize <<= 1;
      t++;
    }

    const eq = this.#equalize;
    for (let i = 0; i < outSpec.length; i++) {
      outSpec[i] = Math.sqrt(real[i] * real[i] + imag[i] * imag[i]) * eq[i];
    }
  }
}

class WinampElement extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // SIGNALS - UI

  #marqueeOverride = signal(/** @type {string | null} */ (null));
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #marqueeOverrideTimeout = undefined;
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #isLoadingTimeout = undefined;
  #playlist = signal(
    /** @type {{ past: Item[]; now: Item | null; future: Item[] }} */ ({
      past: [],
      now: null,
      future: [],
    }),
  );
  /** @type {ReturnType<typeof setTimeout> | undefined} */
  #playlistDebounce = undefined;
  #dragState = signal(
    /** @type {{ fromIdx: number; fromKey: string; toIdx: number; startY: number; multiDrag: boolean } | null} */ (null),
  );
  /** @type {((e: MouseEvent) => void) | null} */
  #dragMouseMove = null;
  /** @type {((e: MouseEvent) => void) | null} */
  #dragMouseUp = null;
  /** @type {((e: KeyboardEvent) => void) | null} */
  #onKeyDown = null;
  #marqueeCurrentOffset = 0;
  /** @type {HTMLElement | null} */
  #marqueeScroller = null;
  /** @type {HTMLElement | null} */
  #playlistHandle = null;

  // Web Audio / Visualizer
  /** @type {AudioContext | null} */
  #audioCtx = null;
  /** @type {GainNode | null} */
  #preampNode = null;
  /** @type {BiquadFilterNode[]} */
  #eqNodes = [];
  /** @type {AnalyserNode | null} */
  #analyser = null;
  /** @type {Map<string, MediaElementAudioSourceNode>} */
  #srcNodes = new Map();
  /** @type {number | undefined} */
  #visRAF = undefined;
  /** @type {ReturnType<typeof setInterval> | undefined} */
  #marqueeStepInterval = undefined;
  #marqueeText = signal("");
  #selectedIndex = signal(/** @type {number | null} */ (null));
  #selectedIndices = signal(/** @type {Set<number>} */ (new Set()));
  #anchorIdx = /** @type {number | null} */ (null);
  #mainOpen = signal(true);
  #eqOpen = signal(true);
  #mainShade = signal(false);
  #eqShade = signal(false);
  #playlistShade = signal(false);
  #eqOn = signal(false);
  #eqSliders = signal({
    preamp: 50,
    bands: /** @type {number[]} */ (Array(10).fill(50)),
  });
  #balance = signal(0);
  #stopped = signal(false);
  #seekingProgress = signal(/** @type {number | null} */ (null));
  #timeCountdown = signal(false);
  #focusedWindow = signal(
    /** @type {"main" | "eq" | "playlist" | "milkdrop"} */ ("main"),
  );
  #playlistOpen = signal(true);
  #milkdropOpen = signal(true);

  // Window positions — plain objects (not signals) so dragging doesn't
  // trigger re-renders, but the current value is always read on each render.
  #mainPos = { x: 0, y: 0 };
  #eqPos = { x: 0, y: 116 };
  #playlistPos = { x: 0, y: 232 };
  #playlistSize = { width: 275, height: 232 };
  #milkdropPos = { x: 275, y: 0 };
  #milkdropSize = { width: 275, height: 232 };

  // Butterchurn
  /** @type {any} */
  #butterchurn = null;
  /** @type {number | undefined} */
  #butterchurnRAF = undefined;
  /** @type {ReturnType<typeof setInterval> | undefined} */
  #butterchurnCycleInterval = undefined;
  /** @type {Array<any>} */
  #butterchurnPresetList = [];

  // SIGNALS - DEPENDENCIES

  $controller = signal(
    /** @type {ControllerOrchestrator | undefined} */ (undefined),
  );
  $repeatShuffle = signal(
    /** @type {RepeatShuffleEngine | undefined} */ (undefined),
  );

  // SIGNALS - COMPUTED

  audio = () => this.$controller.value?.audio();
  currentTrack = () => this.$controller.value?.currentTrack();
  isPlaying = () => this.$controller.value?.isPlaying();

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {ControllerOrchestrator} */
    const controller = query(this, "controller-orchestrator-selector");

    /** @type {RepeatShuffleEngine} */
    const repeatShuffle = query(this, "repeat-shuffle-engine-selector");

    whenElementsDefined({ controller, repeatShuffle }).then(() => {
      this.$controller.value = controller;
      this.$repeatShuffle.value = repeatShuffle;

      this.effect(() => {
        const queueEl = this.$controller.value?.$queue.value;
        const past = queueEl?.past() ?? [];
        const now = queueEl?.now() ?? null;
        const future = queueEl?.future() ?? [];
        clearTimeout(this.#playlistDebounce);
        this.#playlistDebounce = setTimeout(() => {
          this.#playlist.value = { past, now, future };
        }, 100);
      });

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

      this.effect(() => {
        const audioId = this.$controller.value?.$queue.value?.now()?.id;
        const playing = this.isPlaying();
        untracked(() => {
          if (audioId) this.#connectToAnalyser(audioId);
          if (playing) {
            this.#ensureAnalyser();
            this.#audioCtx?.resume();
            this.#startVisualizer();
          }
        });
      });

      this.effect(() => {
        const now = !!this.$controller.value?.$queue.value?.now();
        const loadingState = this.audio()?.loadingState();
        const isError = now && typeof loadingState === "object" &&
          loadingState !== null && "error" in loadingState;
        const isLoading = now && !isError && loadingState !== "loaded";

        clearTimeout(this.#isLoadingTimeout);

        if (isError) {
          this.#marqueeOverride.value = "Audio error ...";
        } else if (isLoading) {
          this.#isLoadingTimeout = setTimeout(() => {
            this.#marqueeOverride.value = "Loading audio ...";
          }, 2000);
        } else if (
          this.#marqueeOverride.value === "Loading audio ..." ||
          this.#marqueeOverride.value === "Audio error ..."
        ) {
          this.#marqueeOverride.value = null;
        }
      });
    });

    // UI State
    const ui = loadUiState();
    this.#eqOpen.value = ui.eqOpen;
    this.#eqOn.value = ui.eqOn;
    this.#mainShade.value = ui.mainShade;
    this.#eqShade.value = ui.eqShade;
    this.#playlistShade.value = ui.playlistShade;
    this.#playlistOpen.value = ui.playlistOpen;
    this.#milkdropOpen.value = ui.milkdropOpen;
    if (ui.eqSliders) {
      const s = ui.eqSliders;
      this.#eqSliders.value = {
        preamp: s["preamp"] ?? 50,
        bands: EQ_BANDS.map((_, i) => s[`band_${i}`] ?? 50),
      };
    }

    if (ui.sizes) {
      if (ui.sizes.playlist) {
        Object.assign(this.#playlistSize, ui.sizes.playlist);
      }
      if (ui.sizes.milkdrop) {
        Object.assign(this.#milkdropSize, ui.sizes.milkdrop);
      }
    }

    if (ui.positions) {
      if (ui.positions.main) Object.assign(this.#mainPos, ui.positions.main);
      if (ui.positions.eq) Object.assign(this.#eqPos, ui.positions.eq);
      if (ui.positions.playlist) {
        Object.assign(this.#playlistPos, ui.positions.playlist);
      }
      if (ui.positions.milkdrop) {
        Object.assign(this.#milkdropPos, ui.positions.milkdrop);
      }
    } else {
      // Center the windows on startup
      const leftColH = 116 + 116 + this.#playlistSize.height;
      const milkdropOpen = this.#milkdropOpen.value;
      const totalW = milkdropOpen ? 275 + this.#milkdropSize.width : 275;
      const totalH = milkdropOpen
        ? Math.max(leftColH, this.#milkdropSize.height)
        : leftColH;
      const cx = Math.round((window.innerWidth - totalW) / 2);
      const cy = Math.round((window.innerHeight - totalH) / 2);
      this.#mainPos.x = cx;
      this.#mainPos.y = cy;
      this.#eqPos.x = cx;
      this.#eqPos.y = cy + 116;
      this.#playlistPos.x = cx;
      this.#playlistPos.y = cy + 232;
      this.#milkdropPos.x = cx + 275;
      this.#milkdropPos.y = cy;
    }

    this.effect(() => {
      if (!this.#milkdropOpen.value) {
        untracked(() => this.#stopButterchurn());
        return;
      }
      untracked(() => {
        requestAnimationFrame(() => {
          const canvas = this.root().querySelector("#milkdrop-canvas");
          if (!(canvas instanceof HTMLCanvasElement)) return;
          if (!this.#butterchurn) {
            this.#initButterchurn(canvas);
          } else {
            this.#startButterchurn();
          }
        });
      });
    });

    this.forceRender();
    this.#marqueeScroller = this.root().querySelector("#marquee > div");
    this.#playlistHandle = this.root().querySelector(
      ".playlist-scrollbar-handle",
    );
    requestAnimationFrame(() => {
      this.#drawEqGraph();
      this.#updatePlaylistHandle();
    });

    // Custom playlist scrollbar
    const playlistContent = this.root().querySelector(
      ".playlist-middle-center",
    );
    const playlistHandle = this.root().querySelector(
      ".playlist-scrollbar-handle",
    );
    const playlistScrollbar = this.root().querySelector(".playlist-scrollbar");
    if (playlistContent instanceof HTMLElement) {
      playlistContent.addEventListener(
        "scroll",
        () => this.#updatePlaylistHandle(),
      );
    }
    if (
      playlistHandle instanceof HTMLElement &&
      playlistScrollbar instanceof HTMLElement &&
      playlistContent instanceof HTMLElement
    ) {
      playlistHandle.addEventListener("mousedown", (e) => {
        e.preventDefault();
        const TRACK_H = 13;
        const HANDLE_H = 18;
        const range = playlistScrollbar.clientHeight - HANDLE_H;
        const startY = e.clientY;
        const startTop = parseFloat(playlistHandle.style.top) || 0;
        const maxScroll = playlistContent.scrollHeight -
          playlistContent.clientHeight;
        /** @param {MouseEvent} mv */
        const onMove = (mv) => {
          const newTop = Math.max(
            0,
            Math.min(range, startTop + mv.clientY - startY),
          );
          const rawScroll = (newTop / range) * maxScroll;
          playlistContent.scrollTop = Math.round(rawScroll / TRACK_H) * TRACK_H;
        };
        const onUp = () => {
          document.removeEventListener("mousemove", onMove);
          document.removeEventListener("mouseup", onUp);
        };
        document.addEventListener("mousemove", onMove);
        document.addEventListener("mouseup", onUp);
      });
    }

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

    // winamp-active press feedback + window focus via event delegation
    this.root().addEventListener("pointerdown", (e) => {
      if (!(e.target instanceof HTMLElement)) return;
      // Window focus
      const win = e.target.closest(
        "#main-window, #equalizer-window, #playlist-window, #playlist-window-shade, #milkdrop-window",
      );
      if (win instanceof HTMLElement) {
        if (win.id === "main-window") this.#focusedWindow.value = "main";
        else if (win.id === "equalizer-window") {
          this.#focusedWindow.value = "eq";
        } else if (win.id === "milkdrop-window") {
          this.#focusedWindow.value = "milkdrop";
        } else this.#focusedWindow.value = "playlist";
      }
      // Press feedback
      if (e.target.tagName !== "DIV" || !e.target.id) return;
      const el = e.target;
      el.classList.add("winamp-active");
      const cleanup = () => {
        el.classList.remove("winamp-active");
        document.removeEventListener("pointerup", cleanup);
      };
      document.addEventListener("pointerup", cleanup);
    });

    // Delete selected playlist tracks via keyboard
    this.#onKeyDown = (e) => {
      if (this.#focusedWindow.value !== "playlist") return;
      if (e.key !== "Delete" && e.key !== "Backspace") return;
      e.preventDefault();
      this.#removeTrack();
    };
    document.addEventListener("keydown", this.#onKeyDown);

    // Window dragging
    this.root().addEventListener(
      "mousedown",
      /** @type {EventListener} */ (this.#onWindowDragStart),
    );
  }

  /** @override */
  disconnectedCallback() {
    clearInterval(this.#marqueeStepInterval);
    if (this.#onKeyDown) document.removeEventListener("keydown", this.#onKeyDown);
    this.root().removeEventListener(
      "mousedown",
      /** @type {EventListener} */ (this.#onWindowDragStart),
    );
    if (this.#visRAF !== undefined) {
      cancelAnimationFrame(this.#visRAF);
      this.#visRAF = undefined;
    }
    this.#stopButterchurn();
  }

  // WINDOW SNAPPING — ported from webamp/js/snapUtils.ts

  static #SNAP_DISTANCE = 15;

  /** @param {number} a @param {number} b */
  static #near(a, b) {
    return Math.abs(a - b) < WinampElement.#SNAP_DISTANCE;
  }

  /**
   * @param {{ x: number, y: number, width: number, height: number }} a
   * @param {{ x: number, y: number, width: number, height: number }} b
   */
  static #overlapX(a, b) {
    const D = WinampElement.#SNAP_DISTANCE;
    return a.x <= b.x + b.width + D && b.x <= a.x + a.width + D;
  }

  /**
   * @param {{ x: number, y: number, width: number, height: number }} a
   * @param {{ x: number, y: number, width: number, height: number }} b
   */
  static #overlapY(a, b) {
    const D = WinampElement.#SNAP_DISTANCE;
    return a.y <= b.y + b.height + D && b.y <= a.y + a.height + D;
  }

  /**
   * Returns snapped position of `a` toward `b`, if close enough.
   * @param {{ x: number, y: number, width: number, height: number }} a
   * @param {{ x: number, y: number, width: number, height: number }} b
   */
  static #snapTo(a, b) {
    const near = WinampElement.#near;
    let x, y;
    if (WinampElement.#overlapY(a, b)) {
      if (near(a.x, b.x + b.width)) x = b.x + b.width;
      else if (near(a.x + a.width, b.x)) x = b.x - a.width;
      else if (near(a.x, b.x)) x = b.x;
      else if (near(a.x + a.width, b.x + b.width)) x = b.x + b.width - a.width;
    }
    if (WinampElement.#overlapX(a, b)) {
      if (near(a.y, b.y + b.height)) y = b.y + b.height;
      else if (near(a.y + a.height, b.y)) y = b.y - a.height;
      else if (near(a.y, b.y)) y = b.y;
      else if (near(a.y + a.height, b.y + b.height)) {
        y = b.y + b.height - a.height;
      }
    }
    return { x, y };
  }

  /**
   * @param {{ x: number, y: number, width: number, height: number }} a
   * @param {{ x: number, y: number, width: number, height: number }[]} others
   */
  static #snapToMany(a, others) {
    let x, y;
    for (const b of others) {
      const s = WinampElement.#snapTo(a, b);
      x ??= s.x;
      y ??= s.y;
    }
    return { x, y };
  }

  /**
   * Two windows are "attached" if their edges are already touching (≤1px).
   * @param {{ x: number, y: number, width: number, height: number }} a
   * @param {{ x: number, y: number, width: number, height: number }} b
   */
  static #areTouching(a, b) {
    const T = 1;
    const hTouch = Math.abs(a.x - (b.x + b.width)) <= T ||
      Math.abs(b.x - (a.x + a.width)) <= T;
    const vTouch = Math.abs(a.y - (b.y + b.height)) <= T ||
      Math.abs(b.y - (a.y + a.height)) <= T;
    const oX = a.x < b.x + b.width + T && b.x < a.x + a.width + T;
    const oY = a.y < b.y + b.height + T && b.y < a.y + a.height + T;
    return (hTouch && oY) || (vTouch && oX);
  }

  // WINDOW DRAGGING & RESIZING

  /** @param {MouseEvent} e */
  #onWindowDragStart = (e) => {
    if (!(e.target instanceof HTMLElement)) return;

    // Resize handle takes priority
    if (e.target.id === "playlist-resize-target") {
      this.#onPlaylistResizeStart(e);
      return;
    }
    if (e.target.id === "gen-resize-target") {
      this.#onMilkdropResizeStart(e);
      return;
    }

    const draggable = e.target.closest(".draggable");
    if (!draggable) return;
    const win = draggable.closest(
      "#main-window, #equalizer-window, #playlist-window, #milkdrop-window",
    );
    if (!(win instanceof HTMLElement)) return;

    e.preventDefault();

    const allWindows = this.#windowEntries();
    const draggedEntry = allWindows.find((w) => w.el === win);
    if (!draggedEntry) return;

    // Only the main window drags attached windows along with it
    /** @type {Set<typeof allWindows[0]>} */
    const attached = new Set();
    if (win.id === "main-window") {
      const trace = (/** @type {typeof allWindows[0]} */ entry) => {
        for (const other of allWindows) {
          if (
            other === entry || other === draggedEntry || attached.has(other)
          ) continue;
          if (WinampElement.#areTouching(entry.box(), other.box())) {
            attached.add(other);
            trace(other);
          }
        }
      };
      trace(draggedEntry);
    }

    const startMouseX = e.clientX;
    const startMouseY = e.clientY;
    const startPos = { x: draggedEntry.pos.x, y: draggedEntry.pos.y };
    const attachedStarts = [...attached].map((w) => ({
      w,
      x: w.pos.x,
      y: w.pos.y,
    }));
    const snapTargets = allWindows
      .filter((w) => w !== draggedEntry && !attached.has(w))
      .map((w) => w.box());

    /** @param {MouseEvent} mv */
    const onMove = (mv) => {
      const dx = mv.clientX - startMouseX;
      const dy = mv.clientY - startMouseY;
      const newX = startPos.x + dx;
      const newY = startPos.y + dy;

      const snapped = WinampElement.#snapToMany(
        { ...draggedEntry.box(), x: newX, y: newY },
        snapTargets,
      );
      const finalX = snapped.x ?? newX;
      const finalY = snapped.y ?? newY;

      draggedEntry.pos.x = finalX;
      draggedEntry.pos.y = finalY;
      win.style.left = `${finalX}px`;
      win.style.top = `${finalY}px`;

      const snapDx = finalX - newX;
      const snapDy = finalY - newY;
      for (const { w, x: sx, y: sy } of attachedStarts) {
        w.pos.x = sx + dx + snapDx;
        w.pos.y = sy + dy + snapDy;
        w.el.style.left = `${w.pos.x}px`;
        w.el.style.top = `${w.pos.y}px`;
      }
    };

    const onUp = () => {
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
      this.forceRender();
      this.#saveLayout();
    };

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  };

  /** @param {MouseEvent} e */
  #onMilkdropResizeStart = (e) => {
    e.preventDefault();

    const milkdropEl = this.root().querySelector("#milkdrop-window");
    if (!(milkdropEl instanceof HTMLElement)) return;

    const startMouseX = e.clientX;
    const startMouseY = e.clientY;
    const startWidth = this.#milkdropSize.width;
    const startHeight = this.#milkdropSize.height;
    const STEP_W = 25, STEP_H = 29, MIN_W = 275, MIN_H = 116;

    /** @param {MouseEvent} mv */
    const onMove = (mv) => {
      const newWidth = Math.max(
        MIN_W,
        startWidth + Math.round((mv.clientX - startMouseX) / STEP_W) * STEP_W,
      );
      const newHeight = Math.max(
        MIN_H,
        startHeight + Math.round((mv.clientY - startMouseY) / STEP_H) * STEP_H,
      );
      this.#milkdropSize.width = newWidth;
      this.#milkdropSize.height = newHeight;
      milkdropEl.style.width = `${newWidth}px`;
      milkdropEl.style.height = `${newHeight}px`;
      const canvas = this.root().querySelector("#milkdrop-canvas");
      if (canvas instanceof HTMLCanvasElement) {
        const cw = canvas.clientWidth;
        const ch = canvas.clientHeight;
        canvas.width = cw;
        canvas.height = ch;
        this.#butterchurn?.setRendererSize(cw, ch);
      }
    };

    const onUp = () => {
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
      this.#saveLayout();
    };

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  };

  /** @param {MouseEvent} e */
  #onPlaylistResizeStart = (e) => {
    e.preventDefault();

    const playlistEl = this.root().querySelector("#playlist-window");
    if (!(playlistEl instanceof HTMLElement)) return;

    const startMouseX = e.clientX;
    const startMouseY = e.clientY;
    const startWidth = this.#playlistSize.width;
    const startHeight = this.#playlistSize.height;

    // Webamp resize segment: 25px wide, 29px tall
    const STEP_W = 25, STEP_H = 29, MIN_W = 275, MIN_H = 116;

    /** @param {MouseEvent} mv */
    const onMove = (mv) => {
      const newWidth = Math.max(
        MIN_W,
        startWidth + Math.round((mv.clientX - startMouseX) / STEP_W) * STEP_W,
      );
      const newHeight = Math.max(
        MIN_H,
        startHeight + Math.round((mv.clientY - startMouseY) / STEP_H) * STEP_H,
      );
      this.#playlistSize.width = newWidth;
      this.#playlistSize.height = newHeight;
      playlistEl.style.width = `${newWidth}px`;
      playlistEl.style.height = `${newHeight}px`;
    };

    const onUp = () => {
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
      this.#saveLayout();
    };

    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  };

  #updatePlaylistHandle() {
    if (!this.#playlistHandle?.isConnected) {
      this.#playlistHandle = this.root().querySelector(
        ".playlist-scrollbar-handle",
      );
    }
    const handle = this.#playlistHandle;
    const content = this.root().querySelector(".playlist-middle-center");
    const scrollbar = this.root().querySelector(".playlist-scrollbar");
    if (
      !handle || !(content instanceof HTMLElement) ||
      !(scrollbar instanceof HTMLElement)
    ) return;

    const HANDLE_H = 18;
    const range = Math.max(0, scrollbar.clientHeight - HANDLE_H);
    const scrollFraction = content.scrollHeight > content.clientHeight
      ? content.scrollTop / (content.scrollHeight - content.clientHeight)
      : 0;
    handle.style.top = `${scrollFraction * range}px`;
  }

  /**
   * Returns live box descriptors for all three windows.
   */
  #windowEntries() {
    const root = this.root();
    const ps = this.#playlistSize;
    return [
      {
        el: /** @type {HTMLElement} */ (root.querySelector("#main-window")),
        pos: this.#mainPos,
        box: () => ({
          x: this.#mainPos.x,
          y: this.#mainPos.y,
          width: 275,
          height: this.#mainShade.value ? 14 : 116,
        }),
      },
      {
        el:
          /** @type {HTMLElement} */ (root.querySelector("#equalizer-window")),
        pos: this.#eqPos,
        box: () => ({
          x: this.#eqPos.x,
          y: this.#eqPos.y,
          width: 275,
          height: this.#eqShade.value ? 14 : 116,
        }),
      },
      {
        el: /** @type {HTMLElement} */ (root.querySelector("#playlist-window")),
        pos: this.#playlistPos,
        box: () => ({
          x: this.#playlistPos.x,
          y: this.#playlistPos.y,
          width: ps.width,
          height: ps.height,
        }),
      },
      {
        el: /** @type {HTMLElement} */ (root.querySelector("#milkdrop-window")),
        pos: this.#milkdropPos,
        box: () => ({
          x: this.#milkdropPos.x,
          y: this.#milkdropPos.y,
          width: this.#milkdropSize.width,
          height: this.#milkdropSize.height,
        }),
      },
    ].filter((e) => e.el != null);
  }

  // MARQUEE

  static #MARQUEE_MAX_LENGTH = 31;
  static #MARQUEE_SEPARATOR = "  ***  ";
  static #CHAR_WIDTH = 5;

  /** @param {number} n @param {number} m */
  static #marqueeMod(n, m) {
    return ((n % m) + m) % m;
  }

  /** @param {string} text */
  static #marqueeLoopText(text) {
    const MAX = WinampElement.#MARQUEE_MAX_LENGTH;
    return text.length >= MAX
      ? `${text}${WinampElement.#MARQUEE_SEPARATOR}${text}`
      : text.padEnd(MAX, " ");
  }

  // SPECTRUM VISUALIZER

  // Webamp default viscolors[0..15]: y=0 top (black), y=2 loud (red), y=15 quiet (green)
  static #BAR_COLORS = [
    "rgb(0,0,0)", // 0 — black (never visible)
    "rgb(24,33,41)", // 1 — grid dot color (never visible with pushDown=2)
    "rgb(239,49,16)", // 2 — bright red (loud)
    "rgb(206,41,16)", // 3
    "rgb(214,90,0)", // 4
    "rgb(214,102,0)", // 5
    "rgb(214,115,0)", // 6
    "rgb(198,123,8)", // 7
    "rgb(222,165,24)", // 8
    "rgb(214,181,33)", // 9
    "rgb(189,222,41)", // 10
    "rgb(148,222,33)", // 11
    "rgb(41,206,16)", // 12
    "rgb(50,190,16)", // 13
    "rgb(57,181,16)", // 14
    "rgb(49,156,8)", // 15 — dim green (quiet)
  ];

  #ensureAnalyser() {
    if (this.#analyser) return;
    this.#audioCtx = new AudioContext();

    this.#preampNode = this.#audioCtx.createGain();
    this.#eqNodes = EQ_BANDS.map((freq) => {
      const f = /** @type {AudioContext} */ (this.#audioCtx)
        .createBiquadFilter();
      f.type = "peaking";
      f.frequency.value = freq;
      f.Q.value = 1.0;
      f.gain.value = 0;
      return f;
    });
    this.#analyser = this.#audioCtx.createAnalyser();
    this.#analyser.fftSize = 1024;
    this.#analyser.smoothingTimeConstant = 0;

    // Chain: source → preamp → eq[0..9] → analyser → destination
    this.#preampNode.connect(this.#eqNodes[0]);
    for (let i = 0; i < this.#eqNodes.length - 1; i++) {
      this.#eqNodes[i].connect(this.#eqNodes[i + 1]);
    }
    this.#eqNodes[this.#eqNodes.length - 1].connect(this.#analyser);
    this.#analyser.connect(this.#audioCtx.destination);

    this.#applyEq();
  }

  /** @param {string} audioId */
  #connectToAnalyser(audioId) {
    if (this.#srcNodes.has(audioId)) return;
    const audioEl = this.$controller.value?.$audio.value
      ?.querySelector(`de-audio-item[id="${audioId}"]:not([preload]) audio`);
    if (!(audioEl instanceof HTMLAudioElement)) return;
    this.#ensureAnalyser();
    const src = /** @type {AudioContext} */ (this.#audioCtx)
      .createMediaElementSource(audioEl);
    src.connect(/** @type {GainNode} */ (this.#preampNode));
    this.#srcNodes.set(audioId, src);
  }

  #startVisualizer() {
    if (this.#visRAF !== undefined) return;
    const canvas = this.root().querySelector("#visualizer");
    if (!(canvas instanceof HTMLCanvasElement)) return;
    const ctx = canvas.getContext("2d");
    if (!ctx || !this.#analyser) return;

    ctx.imageSmoothingEnabled = false;

    const MAX_WIDTH = 75;
    const FALL_RATE = 12 / 16;
    const PEAK_FALLOFF = 1.1;
    const analyser = this.#analyser;

    // Webamp FFT (Nullsoft implementation)
    const fft = new WinampFFT();
    const inWaveData = new Float32Array(1024);
    const outSpectralData = new Float32Array(512);
    const timeDomainBuf = new Uint8Array(1024);

    // Per-bar state (matches webamp's BarPaintHandler)
    const sample = new Float32Array(MAX_WIDTH);
    const saFalloff = new Float32Array(MAX_WIDTH);
    const saPeaks = new Int16Array(MAX_WIDTH);
    const saData2 = new Float32Array(MAX_WIDTH);
    const barPeak = new Float32Array(MAX_WIDTH);

    // Cached off-screen canvases — rebuilt when canvas dimensions change
    const bgCanvas = document.createElement("canvas");
    const bgCtx = bgCanvas.getContext("2d");
    const gradCanvas = document.createElement("canvas");
    gradCanvas.width = 1;
    const gradCtx = gradCanvas.getContext("2d");
    const peakCanvas = document.createElement("canvas");
    peakCanvas.width = 1;
    peakCanvas.height = 1;
    const peakCtx = peakCanvas.getContext("2d");
    if (peakCtx) {
      peakCtx.fillStyle = "rgb(150,150,150)";
      peakCtx.fillRect(0, 0, 1, 1);
    }

    let cachedH = 0;
    let cachedW = 0;

    /** @param {number} W @param {number} H */
    const rebuildCaches = (W, H) => {
      bgCanvas.width = W;
      bgCanvas.height = H;
      if (bgCtx) {
        bgCtx.fillStyle = "rgb(0,0,0)";
        bgCtx.fillRect(0, 0, W, H);
        bgCtx.fillStyle = "rgb(24,33,41)";
        for (let x = 0; x < W; x += 2) {
          for (let y = 1; y < H; y += 2) {
            bgCtx.fillRect(x, y, 1, 1);
          }
        }
      }
      gradCanvas.height = H;
      if (gradCtx) {
        const maxColorIdx = WinampElement.#BAR_COLORS.length - 1;
        for (let y = 0; y < H; y++) {
          const colorIdx = H > 1 ? Math.round((y / (H - 1)) * maxColorIdx) : 0;
          gradCtx.fillStyle = WinampElement.#BAR_COLORS[colorIdx] ??
            "rgb(0,0,0)";
          gradCtx.fillRect(0, y, 1, 1);
        }
      }
      cachedH = H;
      cachedW = W;
    };

    const logMaxFreqIndex = Math.log10(512);
    const scale = 0.91;

    const step = () => {
      this.#visRAF = requestAnimationFrame(step);

      const W = canvas.width;
      const H = canvas.height;
      if (W !== cachedW || H !== cachedH) rebuildCaches(W, H);

      // Height-dependent constants
      const FULL_HEIGHT = 15; // reference height (16px canvas)
      const MAX_HEIGHT = H - 1;
      const HEIGHT_SCALE = MAX_HEIGHT / FULL_HEIGHT;
      const PUSH_DOWN = H >= 16 ? 2 : 0;

      // Lazy-connect current audio
      const audioId = this.$controller.value?.$queue.value?.now()?.id;
      if (audioId) this.#connectToAnalyser(audioId);

      // Time-domain → FFT → spectral data
      analyser.getByteTimeDomainData(timeDomainBuf);
      for (let i = 0; i < 1024; i++) {
        inWaveData[i] = (timeDomainBuf[i] - 128) / 24;
      }
      fft.timeToFrequencyDomain(inWaveData, outSpectralData);

      for (let x = 0; x < MAX_WIDTH; x++) {
        const linIdx = (x / (MAX_WIDTH - 1)) * 511;
        const logIdx = Math.pow(10, (logMaxFreqIndex * x) / (MAX_WIDTH - 1));
        const si = (1.0 - scale) * linIdx + scale * logIdx;
        const i1 = Math.min(511, Math.floor(si));
        const i2 = Math.min(511, Math.ceil(si));
        sample[x] = i1 === i2
          ? outSpectralData[i1]
          : (1 - (si - i1)) * outSpectralData[i1] +
            (si - i1) * outSpectralData[i2];
      }

      ctx.drawImage(bgCanvas, 0, 0);

      for (let x = 0; x < MAX_WIDTH; x++) {
        const chunk = x & ~3;
        const saData = Math.min(
          (((sample[chunk] ?? 0) + (sample[chunk + 1] ?? 0) +
            (sample[chunk + 2] ?? 0) + (sample[chunk + 3] ?? 0)) / 4) *
            HEIGHT_SCALE,
          MAX_HEIGHT,
        );

        if (saPeaks[x] >= MAX_HEIGHT * 256) saPeaks[x] = MAX_HEIGHT * 256;

        saFalloff[x] -= FALL_RATE;
        if (saFalloff[x] < saData) saFalloff[x] = saData;

        if (saPeaks[x] <= Math.round(saFalloff[x] * 256)) {
          saPeaks[x] = saFalloff[x] * 256;
          saData2[x] = 3.0;
        }
        barPeak[x] = saPeaks[x] / 256;
        saPeaks[x] -= Math.round(saData2[x]);
        saData2[x] *= PEAK_FALLOFF;
        if (saPeaks[x] < 0) saPeaks[x] = 0;
        if (Math.round(barPeak[x]) < 1) barPeak[x] = -3;

        if (x === chunk + 3) continue;

        const barHeight = Math.round(saFalloff[x]) - PUSH_DOWN;
        if (barHeight > 0) {
          ctx.drawImage(
            gradCanvas,
            0,
            H - barHeight,
            1,
            barHeight,
            x,
            H - barHeight,
            1,
            barHeight,
          );
        }

        const peakHeight = barPeak[x] + 1 - PUSH_DOWN;
        if (peakHeight > 0) {
          ctx.drawImage(peakCanvas, 0, 0, 1, 1, x, H - peakHeight, 1, 1);
        }
      }
    };

    step();
  }

  // EQ

  #applyEq() {
    if (!this.#preampNode || !this.#eqNodes.length) return;
    const { preamp, bands } = this.#eqSliders.value;
    const on = this.#eqOn.value;
    const preampDB = on ? (preamp - 50) / 50 * 12 : 0;
    this.#preampNode.gain.value = Math.pow(10, preampDB / 20);
    this.#eqNodes.forEach((node, i) => {
      node.gain.value = on ? (bands[i] - 50) / 50 * 12 : 0;
    });
  }

  #saveEqState() {
    const { preamp, bands } = this.#eqSliders.value;
    /** @type {Record<string, number>} */
    const sliders = { preamp };
    bands.forEach((v, i) => {
      sliders[`band_${i}`] = v;
    });
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, eqOn: this.#eqOn.value, eqSliders: sliders }),
    );
  }

  /**
   * @param {MouseEvent} e
   * @param {boolean} isPreamp
   * @param {number} bandIndex
   */
  #startSliderDrag(e, isPreamp, bandIndex = 0) {
    e.preventDefault();
    const RANGE = 52; // px travel for handle (63 - 11)
    const SNAP = 5; // webamp BAND_SNAP_DISTANCE
    const startY = e.clientY;
    const startValue = isPreamp
      ? this.#eqSliders.value.preamp
      : this.#eqSliders.value.bands[bandIndex];
    const startTop = Math.floor((1 - startValue / 100) * RANGE);

    const onMove = (/** @type {MouseEvent} */ mv) => {
      const newTop = Math.max(
        0,
        Math.min(RANGE, startTop + mv.clientY - startY),
      );
      const raw = Math.round((1 - newTop / RANGE) * 100);
      const value = Math.abs(raw - 50) < SNAP ? 50 : raw;
      const cur = this.#eqSliders.value;
      if (isPreamp) {
        this.#eqSliders.value = { ...cur, preamp: value };
      } else {
        const bands = [...cur.bands];
        bands[bandIndex] = value;
        this.#eqSliders.value = { ...cur, bands };
      }
      this.#applyEq();
      this.#drawEqGraph();
    };

    const onUp = () => {
      document.removeEventListener("mousemove", onMove);
      document.removeEventListener("mouseup", onUp);
      this.#saveEqState();
    };
    document.addEventListener("mousemove", onMove);
    document.addEventListener("mouseup", onUp);
  }

  #toggleEqOn = () => {
    this.#eqOn.value = !this.#eqOn.value;
    this.#applyEq();
    this.#saveEqState();
    requestAnimationFrame(() => this.#drawEqGraph());
  };

  #resetEq = () => {
    this.#eqSliders.value = { preamp: 50, bands: Array(10).fill(50) };
    this.#applyEq();
    this.#saveEqState();
    requestAnimationFrame(() => this.#drawEqGraph());
  };

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

    const { bands } = this.#eqSliders.value;
    const H = canvas.height - 1; // 18
    const paddingLeft = 2;
    const totalW = 108; // 9 intervals × 12px

    ctx.clearRect(0, 0, canvas.width, canvas.height);

    for (let x = 0; x <= totalW; x++) {
      // Interpolate between the 10 band values
      const t = (x / totalW) * 9;
      const i1 = Math.min(9, Math.floor(t));
      const i2 = Math.min(9, i1 + 1);
      const v = bands[i1] * (1 - (t - i1)) + bands[i2] * (t - i1);
      const y = Math.round((1 - v / 100) * H);
      const [r, g, b] = WinampElement.#EQ_COLORS[y] ??
        WinampElement.#EQ_COLORS[0];
      ctx.fillStyle = `rgb(${r},${g},${b})`;
      ctx.fillRect(paddingLeft + x, y, 1, 1);
    }
  }

  // EVENTS

  /** @param {Event} e */
  #onVolumeInput = (e) => {
    if (!(e.target instanceof HTMLInputElement)) return;
    const volume = Number(e.target.value) / 100;
    this.$controller.value?.$audio.value?.adjustVolume({ volume });

    this.#marqueeOverride.value = `Volume: ${Math.round(volume * 100)}%`;
    clearTimeout(this.#marqueeOverrideTimeout);
    this.#marqueeOverrideTimeout = setTimeout(() => {
      this.#marqueeOverride.value = null;
    }, 2000);
  };

  /** @param {Event} e */
  #onBalanceInput = (e) => {
    if (!(e.target instanceof HTMLInputElement)) return;
    this.#balance.value = Number(e.target.value);
  };

  /** @param {Event} e */
  #onPositionInput = (e) => {
    if (!(e.target instanceof HTMLInputElement)) return;
    const percentage = Number(e.target.value) / 100;
    this.#seekingProgress.value = percentage;
    const duration = this.audio()?.duration() ?? 0;
    const secs = Math.round(percentage * duration);
    const m = Math.floor(secs / 60);
    const s = secs % 60;
    this.#marqueeOverride.value = `Seek to: ${m}:${
      String(s).padStart(2, "0")
    } (${Math.round(percentage * 100)}%)`;
    clearTimeout(this.#marqueeOverrideTimeout);
  };

  /** @param {Event} e */
  #onPositionChange = (e) => {
    if (!(e.target instanceof HTMLInputElement)) return;
    const percentage = Number(e.target.value) / 100;
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (audioId) {
      this.$controller.value?.$audio.value?.seek({ audioId, percentage });
    }
    this.#marqueeOverride.value = null;
    setTimeout(() => {
      this.#seekingProgress.value = null;
    }, 250);
  };

  #reload = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (audioId) {
      const progress = this.audio()?.progress();
      this.$controller.value?.$audio.value?.reload({ audioId, play: true, progress });
    }
  };

  #playPause = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    const loadingState = this.audio()?.loadingState();
    const isError = typeof loadingState === "object" && loadingState !== null &&
      "error" in loadingState;

    if (isError) {
      this.#reload();
      return;
    }

    this.#stopped.value = false;
    if (this.isPlaying() && audioId) {
      this.$controller.value?.$audio.value?.pause({ audioId });
    } else if (audioId) {
      this.$controller.value?.$audio.value?.play({ audioId });
    }
  };

  #stop = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (!audioId) return;
    if (this.isPlaying()) {
      this.$controller.value?.$audio.value?.pause({ audioId });
    }
    this.$controller.value?.$audio.value?.seek({ audioId, percentage: 0 });
    this.#stopped.value = true;
  };

  #openConnect = () => {
    window.open("l/?path=facets%2Fconnect%2Findex.html", "_blank");
  };

  #next = () => {
    this.$controller.value?.$queue.value?.shift();
  };

  #previous = () => {
    this.$controller.value?.$queue.value?.unshift();
  };

  #toggleShuffle = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setShuffle(!rs.shuffle());
  };

  #toggleRepeat = () => {
    const rs = this.$repeatShuffle.value;
    if (rs) rs.setRepeat(!rs.repeat());
  };

  #centerWindows = () => {
    const leftColH = 116 + 116 + this.#playlistSize.height;
    const milkdropOpen = this.#milkdropOpen.value;
    const totalW = milkdropOpen ? 275 + this.#milkdropSize.width : 275;
    const totalH = milkdropOpen
      ? Math.max(leftColH, this.#milkdropSize.height)
      : leftColH;
    const cx = Math.round((window.innerWidth - totalW) / 2);
    const cy = Math.round((window.innerHeight - totalH) / 2);
    this.#mainPos.x = cx;
    this.#mainPos.y = cy;
    this.#eqPos.x = cx;
    this.#eqPos.y = cy + 116;
    this.#playlistPos.x = cx;
    this.#playlistPos.y = cy + 232;
    this.#milkdropPos.x = cx + 275;
    this.#milkdropPos.y = cy;
    this.forceRender();
    this.#saveLayout();
  };

  #saveLayout = () => {
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({
        ...ui,
        positions: {
          main: { ...this.#mainPos },
          eq: { ...this.#eqPos },
          playlist: { ...this.#playlistPos },
          milkdrop: { ...this.#milkdropPos },
        },
        sizes: {
          playlist: { ...this.#playlistSize },
          milkdrop: { ...this.#milkdropSize },
        },
      }),
    );
  };

  #toggleMilkdrop = () => {
    this.#milkdropOpen.value = !this.#milkdropOpen.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, milkdropOpen: this.#milkdropOpen.value }),
    );
  };

  #toggleMilkdropFullscreen = () => {
    const canvas = this.root().querySelector("#milkdrop-canvas");
    if (!(canvas instanceof HTMLCanvasElement)) return;

    if (document.fullscreenElement) {
      document.exitFullscreen();
    } else {
      canvas.requestFullscreen();
      canvas.addEventListener("fullscreenchange", () => {
        const cw = canvas.clientWidth;
        const ch = canvas.clientHeight;
        canvas.width = cw;
        canvas.height = ch;
        this.#butterchurn?.setRendererSize(cw, ch);
      }, { once: true });
    }
  };

  /** @param {HTMLCanvasElement} canvas */
  #initButterchurn = async (canvas) => {
    this.#ensureAnalyser();
    if (!this.#audioCtx || !this.#analyser) return;

    const { default: butterchurn } = await import("butterchurn");
    const w = canvas.clientWidth || canvas.offsetWidth ||
      this.#milkdropSize.width;
    const h = canvas.clientHeight || canvas.offsetHeight ||
      (this.#milkdropSize.height - 34);
    canvas.width = w;
    canvas.height = h;
    this.#butterchurn = butterchurn.createVisualizer(this.#audioCtx, canvas, {
      width: w,
      height: h,
    });
    this.#butterchurn.connectAudio(this.#analyser);

    const { default: raw } = await import("butterchurn-presets/dist/base.js");
    const presets = typeof raw?.default === "object" && raw.default !== null
      ? raw.default
      : raw;
    this.#butterchurnPresetList = Object.values(presets ?? {});
    this.#cyclePreset(0);

    this.#butterchurnCycleInterval = setInterval(
      () => this.#cyclePreset(5.7),
      15000,
    );
    this.#startButterchurn();
  };

  #startButterchurn = () => {
    if (this.#butterchurnRAF !== undefined) return;
    const step = () => {
      this.#butterchurnRAF = requestAnimationFrame(step);
      if (this.isPlaying()) {
        try {
          this.#butterchurn?.render();
        } catch {
          this.#cyclePreset(0);
        }
      }
    };
    step();
    if (
      this.#butterchurnCycleInterval === undefined &&
      this.#butterchurnPresetList.length
    ) {
      this.#butterchurnCycleInterval = setInterval(
        () => this.#cyclePreset(5.7),
        15000,
      );
    }
  };

  /** @param {number} transitionSecs */
  #cyclePreset = (transitionSecs) => {
    const list = this.#butterchurnPresetList;
    if (!list.length) return;
    const preset = list[Math.floor(Math.random() * list.length)];
    this.#butterchurn?.loadPreset(preset, transitionSecs);
  };

  #stopButterchurn = () => {
    if (this.#butterchurnRAF !== undefined) {
      cancelAnimationFrame(this.#butterchurnRAF);
      this.#butterchurnRAF = undefined;
    }
    if (this.#butterchurnCycleInterval !== undefined) {
      clearInterval(this.#butterchurnCycleInterval);
      this.#butterchurnCycleInterval = undefined;
    }
  };

  #closeMain = () => {
    const audioId = this.$controller.value?.$queue.value?.now()?.id;
    if (audioId && this.isPlaying()) {
      this.$controller.value?.$audio.value?.pause({ audioId });
    }
    this.#mainOpen.value = false;
  };

  open = () => {
    this.#mainOpen.value = true;
  };

  #toggleEq = () => {
    this.#eqOpen.value = !this.#eqOpen.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, eqOpen: this.#eqOpen.value }),
    );
  };

  #toggleTimeCountdown = () => {
    this.#timeCountdown.value = !this.#timeCountdown.value;
  };

  #toggleMainShade = () => {
    this.#mainShade.value = !this.#mainShade.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, mainShade: this.#mainShade.value }),
    );
  };

  #toggleEqShade = () => {
    this.#eqShade.value = !this.#eqShade.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, eqShade: this.#eqShade.value }),
    );
    if (!this.#eqShade.value) {
      requestAnimationFrame(() => this.#drawEqGraph());
    }
  };

  #togglePlaylistShade = () => {
    this.#playlistShade.value = !this.#playlistShade.value;
    const ui = loadUiState();
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...ui, playlistShade: this.#playlistShade.value }),
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

  /** @param {number} idx */
  #selectTrack = (idx, shiftKey = false) => {
    this.#selectedIndex.value = idx;
    if (shiftKey && this.#anchorIdx !== null) {
      const from = Math.min(this.#anchorIdx, idx);
      const to = Math.max(this.#anchorIdx, idx);
      const range = new Set();
      for (let i = from; i <= to; i++) range.add(i);
      this.#selectedIndices.value = range;
    } else {
      this.#anchorIdx = idx;
      this.#selectedIndices.value = new Set([idx]);
    }
  };

  #removeTrack = () => {
    const queue = this.$controller.value?.$queue.value;
    if (!queue) return;
    const indices = [...this.#selectedIndices.value].sort((a, b) => b - a);
    if (indices.length === 0) return;
    const { past, now, future } = this.#playlist.value;
    const pLen = past.length;
    const nLen = now ? 1 : 0;
    const total = pLen + nLen + future.length;
    for (const idx of indices) {
      const item = idx < pLen ? past[idx]
        : idx < pLen + nLen ? now
        : future[idx - pLen - nLen];
      if (item) queue.expel({ key: item.key });
    }
    const newTotal = total - indices.length;
    const minIdx = Math.min(...indices);
    this.#selectedIndices.value = new Set();
    this.#selectedIndex.value = newTotal <= 0 ? null : Math.min(minIdx, newTotal - 1);
  };

  static #TRACK_HEIGHT = 13;

  /**
   * @param {MouseEvent} e
   * @param {number} idx
   * @param {string} key
   * @param {number} totalItems
   */
  #onTrackMouseDown = (e, idx, key, totalItems) => {
    e.preventDefault();
    const multiDrag = this.#selectedIndices.value.size > 1 &&
      this.#selectedIndices.value.has(idx);
    this.#dragState.value = { fromIdx: idx, fromKey: key, toIdx: idx, startY: e.clientY, multiDrag };

    this.#dragMouseMove = (mv) => {
      const state = this.#dragState.value;
      if (!state) return;
      const diff = Math.round(
        (mv.clientY - state.startY) / WinampElement.#TRACK_HEIGHT,
      );
      let toIdx;
      if (state.multiDrag) {
        const sorted = [...this.#selectedIndices.value].sort((a, b) => a - b);
        const clampedDiff = Math.max(
          -sorted[0],
          Math.min(totalItems - 1 - sorted[sorted.length - 1], diff),
        );
        toIdx = state.fromIdx + clampedDiff;
      } else {
        toIdx = Math.max(0, Math.min(totalItems - 1, state.fromIdx + diff));
      }
      if (toIdx !== state.toIdx) this.#dragState.value = { ...state, toIdx };
    };

    this.#dragMouseUp = () => {
      const state = this.#dragState.value;
      if (state && state.fromIdx !== state.toIdx) {
        const { past, now, future } = this.#playlist.value;
        const all = [...past, ...(now ? [now] : []), ...future];
        const queue = this.$controller.value?.$queue.value;

        if (state.multiDrag) {
          const sortedSel = [...this.#selectedIndices.value].sort((a, b) => a - b);
          const delta = state.toIdx - state.fromIdx; // already clamped by mousemove

          // Build new array: each selected item shifts by delta, non-selected fill gaps
          const selectedSet = new Set(sortedSel);
          const result = new Array(all.length).fill(null);
          sortedSel.forEach((i) => { result[i + delta] = all[i]; });
          const nonSelected = all.filter((_, i) => !selectedSet.has(i));
          let nsIdx = 0;
          for (let i = 0; i < result.length; i++) {
            if (result[i] === null) result[i] = nonSelected[nsIdx++];
          }

          // Commit moves to queue (order matters to preserve relative positions)
          if (queue) {
            const processOrder = delta > 0 ? [...sortedSel].reverse() : sortedSel;
            for (const origIdx of processOrder) {
              queue.move({ key: all[origIdx].key, to: origIdx + delta });
            }
          }

          const nowIdx = now ? result.indexOf(now) : past.length;
          clearTimeout(this.#playlistDebounce);
          this.#anchorIdx = state.toIdx;
          batch(() => {
            this.#dragState.value = null;
            this.#playlist.value = {
              past: result.slice(0, nowIdx),
              now: now ? (result[nowIdx] ?? null) : null,
              future: result.slice(nowIdx + (now ? 1 : 0)),
            };
            this.#selectedIndices.value = new Set(sortedSel.map((i) => i + delta));
            this.#selectedIndex.value = state.toIdx;
          });
        } else {
          if (queue) queue.move({ key: state.fromKey, to: state.toIdx });
          const [item] = all.splice(state.fromIdx, 1);
          all.splice(state.toIdx, 0, item);
          const nowIdx = now ? all.indexOf(now) : past.length;
          clearTimeout(this.#playlistDebounce);
          this.#anchorIdx = state.toIdx;
          batch(() => {
            this.#dragState.value = null;
            this.#playlist.value = {
              past: all.slice(0, nowIdx),
              now: now ? (all[nowIdx] ?? null) : null,
              future: all.slice(nowIdx + (now ? 1 : 0)),
            };
            this.#selectedIndices.value = new Set([state.toIdx]);
            this.#selectedIndex.value = state.toIdx;
          });
        }
      } else {
        this.#dragState.value = null;
      }
      if (this.#dragMouseMove) {
        window.removeEventListener("mousemove", this.#dragMouseMove);
      }
      if (this.#dragMouseUp) {
        window.removeEventListener("mouseup", this.#dragMouseUp);
      }
      this.#dragMouseMove = null;
      this.#dragMouseUp = null;
    };

    window.addEventListener("mousemove", this.#dragMouseMove);
    window.addEventListener("mouseup", this.#dragMouseUp);
  };

  /** @param {number} idx */
  #playTrack = (idx) => {
    this.#anchorIdx = idx;
    this.#selectedIndices.value = new Set([idx]);
    this.#selectedIndex.value = idx;
    const queue = this.$controller.value?.$queue.value;
    if (!queue) return;
    const past = queue.past();
    const pastLen = past.length;
    if (idx === pastLen) return;
    if (idx < pastLen) {
      const stepsBack = pastLen - idx;
      for (let i = 0; i < stepsBack; i++) queue.unshift();
      return;
    }
    const stepsForward = idx - pastLen;
    for (let i = 0; i < stepsForward; i++) queue.shift();
  };

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    requestAnimationFrame(() => this.#updatePlaylistHandle());

    const { preamp: preampVal, bands: bandVals } = this.#eqSliders.value;

    /**
     * @param {number} value 0–100
     * @param {boolean} isPreamp
     * @param {number} bandIndex
     */
    const bandSlider = (value, isPreamp, bandIndex = 0) => {
      const handleTop = Math.floor((1 - value / 100) * 52);
      const n = Math.round((value / 100) * 27);
      const sx = (n % 14) * 15;
      const sy = Math.floor(n / 14) * 65;
      return html`
        <div
          class="band"
          style="background-position: -${sx}px -${sy}px; width: 14px; height: 63px; position: relative;"
        >
          <div
            class="slider-handle"
            style="position: absolute; top: ${handleTop}px; width: 11px; height: 11px; margin-left: 1px;"
            @mousedown="${(/** @type {MouseEvent} */ e) =>
              this.#startSliderDrag(e, isPreamp, bandIndex)}"
          >
          </div>
        </div>
      `;
    };

    const volume = this.$controller.value?.$audio.value?.volume() ?? 1;
    const volumeSprite = Math.round(volume * 28);
    const volumeBgPos = `0 -${(volumeSprite - 1) * 15}px`;
    const volumePct = Math.round(volume * 100);
    const volumeClass = volumePct < 50
      ? "left"
      : volumePct > 50
      ? "right"
      : "center";
    const balance = this.#balance.value;
    const balanceClass = balance < 0
      ? "left"
      : balance > 0
      ? "right"
      : "center";

    const audio = this.audio();
    const focused = this.#focusedWindow.value;

    // Track metadata
    const track = this.currentTrack();
    const kbps = track?.stats?.bitrate
      ? Math.round(track.stats.bitrate / 1000)
      : null;
    const khz = track?.stats?.sampleRate
      ? Math.round(track.stats.sampleRate / 1000)
      : null;
    const channels = track?.stats?.numberOfChannels ?? null;
    const isStereo = channels !== null && channels >= 2;
    const isMono = channels === 1;
    const kbpsChars = kbps != null
      ? [...String(kbps)].map((c) =>
        html`<span class="character character-${c.charCodeAt(0)}">${c}</span>`
      )
      : [];
    const khzChars = khz != null
      ? [...String(khz)].map((c) =>
        html`<span class="character character-${c.charCodeAt(0)}">${c}</span>`
      )
      : [];

    const seekPct = this.#seekingProgress.value;
    const duration = audio?.duration() ?? 0;
    const elapsed = seekPct !== null ? seekPct * duration : audio?.currentTime() ?? 0;
    const isCountdown = this.#timeCountdown.value;
    const timeSeconds = isCountdown ? Math.max(0, duration - elapsed) : elapsed;
    const timeMinutes = Math.floor(timeSeconds / 60);
    const timeSecs = Math.floor(timeSeconds % 60);
    const miniTimeStr = `${String(timeMinutes).padStart(2, "0")}:${
      String(timeSecs).padStart(2, "0")
    }`;
    const miniTimeChars = [...miniTimeStr].map((c, i) =>
      c === ":" ? null : html`
        <span class="character character-${c.charCodeAt(0)}" style="left: ${i *
          5}px"
        >${c}</span>
      `
    );
    const d = {
      mFirst: Math.floor(timeMinutes / 10),
      mSecond: timeMinutes % 10,
      sFirst: Math.floor(timeSecs / 10),
      sSecond: timeSecs % 10,
    };

    // Playlist
    const queueEl = this.$controller.value?.$queue.value;
    const { past: queuePast, now: nowItem, future: queueFuture } =
      this.#playlist.value;
    const allItems = [
      ...queuePast,
      ...(nowItem ? [nowItem] : []),
      ...queueFuture,
    ];

    // Apply local drag reorder for visual feedback (worker is only called on mouseup)
    const dragState = this.#dragState.value;
    const selectedIndices = this.#selectedIndices.value;
    const displayItems = dragState && dragState.fromIdx !== dragState.toIdx
      ? (() => {
        const arr = [...allItems];
        if (dragState.multiDrag && selectedIndices.size > 1) {
          const sortedSel = [...selectedIndices].sort((a, b) => a - b);
          const delta = dragState.toIdx - dragState.fromIdx;
          const selectedSet = new Set(sortedSel);
          const result = new Array(arr.length).fill(null);
          sortedSel.forEach((i) => { result[i + delta] = arr[i]; });
          const nonSelected = arr.filter((_, i) => !selectedSet.has(i));
          let nsIdx = 0;
          for (let i = 0; i < result.length; i++) {
            if (result[i] === null) result[i] = nonSelected[nsIdx++];
          }
          return result;
        }
        const [item] = arr.splice(dragState.fromIdx, 1);
        arr.splice(dragState.toIdx, 0, item);
        return arr;
      })()
      : allItems;

    const col = this.$controller.value?.$output.value?.tracks.collection();
    const trackMap = col?.state === "loaded"
      ? new Map(col.data.map((t) => [t.id, t]))
      : new Map();
    const selectedItemSet = new Set([...selectedIndices].map((i) => allItems[i]));
    const nowIdx = nowItem ? displayItems.indexOf(nowItem) : -1;
    const playlistRows = displayItems.map((item, i) => {
      const track = trackMap.get(item.id);
      const isCurrent = i === nowIdx;
      const isSelected = dragState
        ? (dragState.multiDrag && selectedIndices.size > 1
          ? selectedItemSet.has(item)
          : i === dragState.toIdx)
        : selectedIndices.has(i);
      const artist = track?.tags?.artist ?? "";
      const title = track?.tags?.title ?? "";
      const label = artist ? `${artist} - ${title}` : title;
      const durSec = track?.stats?.duration ? track.stats.duration / 1000 : 0;
      const dur = durSec > 0
        ? `${Math.floor(durSec / 60)}:${
          String(Math.floor(durSec % 60)).padStart(2, "0")
        }`
        : "";
      const color = isCurrent ? "#FFFFFF" : "#00FF00";
      const bg = isSelected && !isCurrent ? "#0000FF" : "transparent";
      return { id: item.id, item, idx: i, n: i + 1, label, dur, color, bg };
    });

    // Playlist running time display: currentTrackDuration/totalPlaylistDuration
    const nowTrackSec = (() => {
      const d = trackMap.get(nowItem?.id ?? "")?.stats?.duration;
      return d ? d / 1000 : 0;
    })();
    const totalSec = allItems.reduce((sum, item) => {
      const d = trackMap.get(item.id)?.stats?.duration;
      return sum + (d ? d / 1000 : 0);
    }, 0);
    /** @param {number} s */
    const fmtDur = (s) => {
      const h = Math.floor(s / 3600);
      const m = Math.floor((s % 3600) / 60);
      const sec = Math.floor(s % 60);
      return h > 0
        ? `${h}:${String(m).padStart(2, "0")}:${String(sec).padStart(2, "0")}`
        : `${m}:${String(sec).padStart(2, "0")}`;
    };
    const runningTimeStr = `${fmtDur(nowTrackSec)}/${fmtDur(totalSec)}`;
    const totalTimeChars = [...runningTimeStr].map((c) =>
      html`<span class="character character-${c.charCodeAt(0)}">${c}</span>`
    );

    const isPaused = !!audio && !this.isPlaying() && !this.#stopped.value;

    // Playlist mini-time (current playback position)
    const playlistMiniTimeChars = [...miniTimeStr].map((c, i) =>
      c === ":" ? null : html`
        <span class="character character-${c.charCodeAt(0)}" style="left: ${i *
          5}px"
        >${c}</span>
      `
    );

    // Playlist shade: current track title + time
    const nowTrack = trackMap.get(nowItem?.id ?? "");
    const shadeArtist = nowTrack?.tags?.artist ?? "";
    const shadeTitle = shadeArtist
      ? `${shadeArtist} - ${nowTrack?.tags?.title ?? ""}`.toLowerCase()
      : (nowTrack?.tags?.title ?? "").toLowerCase();
    const shadeTitleChars = [...shadeTitle].map((c) =>
      html`<span class="character character-${c.charCodeAt(0)}">${c}</span>`
    );
    const shadeTimeChars = [...miniTimeStr].map((c, i) =>
      c === ":" ? null : html`
        <span class="character character-${c.charCodeAt(0)}" style="left: ${i *
          5}px"
        >${c}</span>
      `
    );

    const activeMarquee = this.#marqueeOverride.value ??
      this.#marqueeText.value;
    const loopedMarquee = WinampElement.#marqueeLoopText(activeMarquee);

    const marqueeChars = [...loopedMarquee].map((char) =>
      html`<span class="character character-${char.toLowerCase().charCodeAt(0)}">${char}</span>`
    );

    return html`
      <style>
      @import "./facets/themes/winamp/vendor/webamp.css";
      @import "./facets/themes/winamp/vendor/gen-window.css";

      #webamp .playlist-track-titles > div {
        padding-left: 3px;
      }

      #webamp .playlist-middle-center {
        scrollbar-width: none;
      }
      #webamp .playlist-middle-center::-webkit-scrollbar {
        display: none;
      }
      #webamp .playlist-middle {
        min-height: 0;
      }
      #webamp .playlist-scrollbar {
        position: absolute;
        top: 0;
        bottom: 0;
        right: 7px;
        width: 8px;
      }
      #webamp .playlist-scrollbar-handle {
        position: absolute;
        top: 0;
        width: 8px;
        height: 18px;
      }
      #webamp #main-window.shade,
      #webamp #equalizer-window.shade {
        overflow: hidden;
      }
      #webamp #main-window.shade #visualizer {
        clip-path: inset(0 38px 0 0);
      }
      #webamp #playlist-window .mini-time {
        left: 72px;
        right: auto;
      }
      #webamp .gen-top-title {
        display: flex;
        margin-top: 4px;
      }
      #webamp .gen-middle-left,
      #webamp .gen-middle-right,
      #webamp .gen-bottom {
        position: relative;
      }
      #webamp #milkdrop-window .gen-middle-center {
        overflow: hidden;
      }
      </style>

      <div id="webamp" style="display: ${this.#mainOpen.value
        ? "block"
        : "none"}">
        <div
          id="main-window"
          class="window ${this.#stopped.value
            ? "stop"
            : this.isPlaying()
            ? "play"
            : audio
            ? "pause"
            : "stop"}${this.#mainShade.value
            ? " shade"
            : ""}${focused === "main" ? " selected" : ""}"
          style="position: absolute; top: ${this.#mainPos.y}px; left: ${this
            .#mainPos.x}px;"
        >
          <div id="title-bar" class="draggable" @dblclick="${this
            .#toggleMainShade}">
            <div id="option-context" @click="${this.#toggleMilkdrop}">
              <div id="option"></div>
            </div>
            <div id="minimize"></div>
            <div id="shade" @click="${this.#toggleMainShade}"></div>
            ${this.#mainShade.value
              ? html`
                <div class="mini-time${isPaused
                  ? " blinking"
                  : ""}">${miniTimeChars}</div>
              `
              : ""}
            <div id="close" @click="${this.#closeMain}"></div>
          </div>
          <div class="webamp-status">
            <div id="clutter-bar">
              <div id="button-o" @click="${this.#centerWindows}"></div>
              <div id="button-a"></div>
              <div id="button-i"></div>
              <div id="button-d"></div>
              <div id="button-v"></div>
            </div>
            <div id="play-pause"></div>
            <div id="work-indicator"></div>
            <div id="time" class="${isCountdown ? "countdown" : ""}" @click="${this.#toggleTimeCountdown}" style="cursor: pointer;">
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
          <canvas id="visualizer" width="76" height="${this.#mainShade.value
            ? 5
            : 16}"></canvas>
          <div class="media-info">
            <div id="marquee">
              <div style="white-space: nowrap; will-change: transform; font-size: 0;">
                ${marqueeChars}
              </div>
            </div>
            <div id="kbps">${kbpsChars}</div>
            <div id="khz">${khzChars}</div>
            <div class="mono-stereo">
              <div id="mono" class="${isMono ? "selected" : ""}"></div>
              <div id="stereo" class="${isStereo ? "selected" : ""}"></div>
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
            .value="${(this.#seekingProgress.value !== null
              ? this.#seekingProgress.value
              : (this.audio()?.progress() ?? 0)) * 100}"
            @input="${this.#onPositionInput}"
            @change="${this.#onPositionChange}"
          >
          <div class="actions">
            <div id="previous" @click="${this.#previous}"></div>
            <div id="play" @click="${this.#playPause}"></div>
            <div id="pause" @click="${this.#playPause}"></div>
            <div id="stop" @click="${this.#stop}"></div>
            <div id="next" @click="${this.#next}"></div>
          </div>
          <div id="eject" @click="${this.#openConnect}"></div>
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
          class="window${this.#eqShade.value ? " shade" : ""}${focused === "eq"
            ? " selected"
            : ""}"
          style="position: absolute; top: ${this.#eqPos.y}px; left: ${this
            .#eqPos.x}px; display: ${this.#eqOpen.value ? "block" : "none"};"
        >
          ${this.#eqShade.value
            ? html`
              <div
                class="draggable"
                style="width: 100%; height: 100%;"
                @dblclick="${this.#toggleEqShade}"
              >
                <div id="equalizer-shade" @click="${this.#toggleEqShade}"></div>
                <div id="equalizer-close" @click="${this.#toggleEq}"></div>
                <input
                  type="range"
                  id="equalizer-volume"
                  class="${volumeClass}"
                  min="0"
                  max="100"
                  value="${volumePct}"
                  @input="${this.#onVolumeInput}"
                >
                <input
                  type="range"
                  id="equalizer-balance"
                  class="${balanceClass}"
                  min="-100"
                  max="100"
                  value="${balance}"
                  @input="${this.#onBalanceInput}"
                >
              </div>
            `
            : html`
              <div class="equalizer-top title-bar draggable" @dblclick="${this
                .#toggleEqShade}">
                <div id="equalizer-shade" @click="${this.#toggleEqShade}"></div>
                <div id="equalizer-close" @click="${this.#toggleEq}"></div>
              </div>
              <div id="on" class="${this.#eqOn.value
                ? "selected"
                : ""}" @click="${this.#toggleEqOn}"></div>
              <div id="auto" @click="${this.#resetEq}"></div>
              <canvas id="eqGraph" width="113" height="19"></canvas>
              <div id="presets-context"><div id="presets"></div></div>
              <div id="preamp">${bandSlider(preampVal, true)}</div>
              <div id="preamp-line"></div>
              <div id="plus12db"></div>
              <div id="zerodb"></div>
              <div id="minus12db"></div>
              ${EQ_BANDS.map((hz, i) =>
                html`
                  <div id="band-${hz}">${bandSlider(
                    bandVals[i],
                    false,
                    i,
                  )}</div>
                `
              )}
            `}
        </div>

        ${this.#playlistShade.value && this.#playlistOpen.value
          ? html`
            <div
              id="playlist-window-shade"
              class="window draggable${focused === "playlist"
                ? " selected"
                : ""}"
              style="position: absolute; top: ${this.#playlistPos
                .y}px; left: ${this.#playlistPos.x}px; width: ${this
                .#playlistSize.width}px;"
            >
              <div class="left">
                <div class="right draggable">
                  <div id="playlist-shade-track-title">${shadeTitleChars}</div>
                  <div id="playlist-shade-time">${shadeTimeChars}</div>
                  <div id="playlist-shade-button" @click="${this
                    .#togglePlaylistShade}"></div>
                  <div id="playlist-close-button" @click="${this
                    .#togglePlaylist}"></div>
                </div>
              </div>
            </div>
          `
          : ""}

        <div
          id="playlist-window"
          class="window${focused === "playlist" ? " selected" : ""}"
          style="position: absolute; top: ${this.#playlistPos.y}px; left: ${this
            .#playlistPos.x}px; height: ${this.#playlistSize
            .height}px; width: ${this.#playlistSize
            .width}px; display: ${this.#playlistOpen.value &&
              !this.#playlistShade.value
            ? "flex"
            : "none"};"
        >
          <div class="playlist-top draggable" @dblclick="${this
            .#togglePlaylistShade}">
            <div class="playlist-top-left draggable"></div>
            <div class="playlist-top-left-fill draggable"></div>
            <div class="playlist-top-title draggable"></div>
            <div class="playlist-top-right-fill draggable"></div>
            <div class="playlist-top-right draggable">
              <div id="playlist-shade-button" @click="${this
                .#togglePlaylistShade}"></div>
              <div id="playlist-close-button" @click="${this
                .#togglePlaylist}"></div>
            </div>
          </div>
          <div class="playlist-middle">
            <div class="playlist-middle-left"></div>
            <div
              class="playlist-middle-center"
              style="background-color: #000000; overflow-y: auto; font-family: Arial, sans-serif;"
            >
              ${guard([
                this.#playlist.value,
                selectedIndices,
                this.#dragState.value,
              ], () =>
                html`
                  <div class="playlist-tracks">
                    <div class="playlist-track-titles">
                      ${repeat(playlistRows, (r) => r.id, (r) =>
                        html`
                          <div
                            class="track-cell"
                            style="color: ${r.color}; background-color: ${r
                              .bg};"
                            @click="${(/** @type {MouseEvent} */ e) => this.#selectTrack(r.idx, e.shiftKey)}"
                            @dblclick="${() => this.#playTrack(r.idx)}"
                            @mousedown="${(/** @type {MouseEvent} */ e) =>
                              this.#onTrackMouseDown(
                                e,
                                r.idx,
                                r.item.key,
                                allItems.length,
                              )}"
                          >
                            ${r.n}. ${r.label}
                          </div>
                        `)}
                    </div>
                    <div class="playlist-track-durations">
                      ${repeat(playlistRows, (r) => r.id, (r) =>
                        html`
                          <div
                            class="track-cell"
                            style="color: ${r.color}; background-color: ${r
                              .bg}; cursor: grab;"
                            @click="${(/** @type {MouseEvent} */ e) => this.#selectTrack(r.idx, e.shiftKey)}"
                            @dblclick="${() => this.#playTrack(r.idx)}"
                            @mousedown="${(/** @type {MouseEvent} */ e) =>
                              this.#onTrackMouseDown(
                                e,
                                r.idx,
                                r.item.key,
                                allItems.length,
                              )}"
                          >
                            ${r.dur}
                          </div>
                        `)}
                    </div>
                  </div>
                `)}
            </div>
            <div class="playlist-middle-right">
              <div class="playlist-scrollbar">
                <div class="playlist-scrollbar-handle"></div>
              </div>
            </div>
          </div>
          <div class="playlist-bottom">
            <div class="playlist-bottom-left">
              <div id="playlist-add-menu" class="playlist-menu" @click="${this
                .#openConnect}">
              </div>
              <div id="playlist-remove-menu" class="playlist-menu" @click="${this.#removeTrack}"></div>
              <div id="playlist-selection-menu" class="playlist-menu"></div>
              <div id="playlist-misc-menu" class="playlist-menu"></div>
            </div>
            <div class="playlist-bottom-center"></div>
            <div class="playlist-bottom-right">
              <div class="playlist-running-time-display draggable">${totalTimeChars}</div>
              <div class="mini-time${isPaused
                ? " blinking"
                : ""}">${playlistMiniTimeChars}</div>
              <div class="playlist-action-buttons">
                <div class="playlist-previous-button" @click="${this
                  .#previous}"></div>
                <div class="playlist-play-button" @click="${this
                  .#playPause}"></div>
                <div class="playlist-pause-button" @click="${this
                  .#playPause}"></div>
                <div class="playlist-stop-button" @click="${this.#stop}"></div>
                <div class="playlist-next-button" @click="${this.#next}"></div>
                <div class="playlist-eject-button" @click="${this
                  .#openConnect}"></div>
              </div>
              <div id="playlist-list-menu" class="playlist-menu"></div>
              <div id="playlist-resize-target"></div>
            </div>
          </div>
        </div>

        <div
          id="milkdrop-window"
          class="window gen-window${this.#focusedWindow.value === "milkdrop"
            ? " selected"
            : ""}"
          style="position: absolute; top: ${this.#milkdropPos.y}px; left: ${this
            .#milkdropPos.x}px; width: ${this.#milkdropSize
            .width}px; height: ${this.#milkdropSize
            .height}px; display: ${this.#milkdropOpen.value ? "flex" : "none"};"
        >
          <div class="gen-top draggable">
            <div class="gen-top-left draggable"></div>
            <div class="gen-top-left-fill draggable"></div>
            <div class="gen-top-left-end draggable"></div>
            <div class="gen-top-title draggable">
              ${"MILKDROP".split("").map((c) =>
                html`
                  <div class="draggable gen-text-letter gen-text-${c
                    .toLowerCase()}"></div>
                `
              )}
            </div>
            <div class="gen-top-right-end draggable"></div>
            <div class="gen-top-right-fill draggable"></div>
            <div class="gen-top-right draggable">
              <div class="gen-close selected" @click="${this
                .#toggleMilkdrop}"></div>
            </div>
          </div>
          <div class="gen-middle">
            <div class="gen-middle-left draggable">
              <div class="gen-middle-left-bottom draggable"></div>
            </div>
            <div class="gen-middle-center" style="background: #000;">
              <canvas
                id="milkdrop-canvas"
                style="position: absolute; inset: 0; width: 100%; height: 100%;"
                @dblclick="${this.#toggleMilkdropFullscreen}"
              ></canvas>
            </div>
            <div class="gen-middle-right draggable">
              <div class="gen-middle-right-bottom draggable"></div>
            </div>
          </div>
          <div class="gen-bottom draggable">
            <div class="gen-bottom-left draggable"></div>
            <div class="gen-bottom-right draggable">
              <div id="gen-resize-target"></div>
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

defineElement(NAME, WinampElement);
