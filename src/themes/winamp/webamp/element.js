import Webamp from "webamp/lazy";

import { batch, effect, signal } from "~/common/signal.js";
import { diff, strictEquality } from "~/common/compare.js";
import DiffuseMedia from "./media.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

const UI_STATE_KEY = "diffuse/themes/winamp/webamp/ui";

/** @returns {{ milkdropOpen: boolean, eqOpen: boolean, playlistOpen: boolean, eqOn: boolean, eqSliders: Record<string, number> | null }} */
function loadUiState() {
  try {
    return {
      milkdropOpen: true,
      eqOpen: true,
      playlistOpen: true,
      eqOn: false,
      eqSliders: null,
      ...JSON.parse(localStorage.getItem(UI_STATE_KEY) ?? "{}"),
    };
  } catch {
    return {
      milkdropOpen: true,
      eqOpen: true,
      playlistOpen: true,
      eqOn: false,
      eqSliders: null,
    };
  }
}

/** @param {Record<string, unknown>} partial */
function saveUiState(partial) {
  try {
    const current = JSON.parse(localStorage.getItem(UI_STATE_KEY) ?? "{}");
    localStorage.setItem(
      UI_STATE_KEY,
      JSON.stringify({ ...current, ...partial }),
    );
  } catch { /* ignore */ }
}

class WebampElement extends HTMLElement {
  constructor() {
    super();

    // ⚡

    const ui = loadUiState();

    /** @type {import("webamp/lazy").default} */
    this.amp = new /** @type {any} */ (Webamp)({
      enableMediaSession: true,
      initialTracks: [],
      zIndex: 99,
      __customMediaClass: DiffuseMedia,
      __butterchurnOptions: {
        importButterchurn: () => import("butterchurn"),
        async getPresets() {
          const { default: raw } = await import(
            // @ts-ignore
            "butterchurn-presets/dist/base.js"
          );

          // In some environments (e.g. Deno's CJS interop), the CJS module.exports
          // is surfaced as the default export, so the actual presets collection is
          // nested under .default.  Unwrap one level if needed.
          const presets =
            typeof raw?.default === "object" && raw.default !== null
              ? raw.default
              : raw;

          return Object.entries(presets ?? {}).map(([name, preset]) => ({
            name,
            butterchurnPresetObject: preset,
          }));
        },
        butterchurnOpen: ui.milkdropOpen,
      },
      windowLayout: {
        main: { position: { top: 0, left: 0 } },
        equalizer: { position: { top: 116, left: 0 }, closed: !ui.eqOpen },
        playlist: {
          position: { top: 232, left: 0 },
          size: { extraHeight: 4, extraWidth: 0 },
          closed: !ui.playlistOpen,
        },
        milkdrop: {
          position: { top: 0, left: 275 },
          size: { extraHeight: 4, extraWidth: 0 },
        },
      },

      /** */
      handleLoadListEvent: async () => {
        // TODO
        return [
          /* Array of Tracks */
        ];
      },

      /**
       * @param {any} tracks
       */
      handleSaveListEvent: (tracks) => {
        // TODO
      },
    });
  }

  // LIFECYCLE

  connectedCallback() {
    this.attachShadow({ mode: "open" });

    // Custom webamp rendering
    this.renderWebamp();
  }

  // ACTIONS

  /**
   * @param {Track} track
   * @param {number} [idx]
   */
  addTrack(track, idx) {
    idx = idx ?? (this.amp.getPlaylistTracks().length);

    this.amp.store.dispatch(
      /**
       * @param {any} dispatch
       */
      (dispatch) => {
        dispatch({
          type: "ADD_TRACK_FROM_URL",
          url: track.uri,
          duration: track.stats?.duration != null
            ? track.stats.duration / 1000
            : undefined,
          defaultName: undefined,
          id: idx,
          atIndex: idx,
        });

        dispatch({
          type: "SET_MEDIA_DURATION",
          duration: track.stats?.duration != null
            ? track.stats.duration / 1000
            : undefined,
          id: idx,
        });

        dispatch({
          type: "SET_MEDIA_TAGS",
          artist: track.tags?.artist,
          title: track.tags?.title,
          album: track.tags?.album,
          sampleRate: track.stats?.sampleRate ?? 44000,
          bitrate: track.stats?.bitrate ?? 192000,
          numberOfChannels: 2, // TODO
          id: idx,
        });
      },
    );
  }

  // RENDER

  async renderWebamp() {
    // Ideally this would render in the shadow root,
    // but sadly it does not.

    const ampNode = document.createElement("main");
    ampNode.style =
      "height: 100vh; left: 0; position: absolute; top: 0; width: 100vw; z-index: -1000;";

    this.shadowRoot?.appendChild(ampNode);

    await this.amp.renderWhenReady(ampNode);

    // Restore EQ settings
    const ui = loadUiState();
    if (ui.eqSliders != null) {
      for (const [band, value] of Object.entries(ui.eqSliders)) {
        // @ts-ignore
        this.amp.store.dispatch({ type: "SET_BAND_VALUE", band, value });
      }
    }

    this.amp.store.dispatch({ type: ui.eqOn ? "SET_EQ_ON" : "SET_EQ_OFF" });

    // Signals for each piece of persisted UI state
    const milkdropOpenSig = signal(ui.milkdropOpen, {
      compare: strictEquality,
    });

    const playlistOpenSig = signal(ui.playlistOpen, {
      compare: strictEquality,
    });

    const eqOnSig = signal(ui.eqOn, { compare: strictEquality });
    const eqOpenSig = signal(ui.eqOpen, { compare: strictEquality });
    const eqSlidersSig = signal(ui.eqSliders, { compare: diff });

    // Update signals in batch on every store change
    this.amp.store.subscribe(() => {
      const state = this.amp.store.getState();
      batch(() => {
        milkdropOpenSig.set(state.windows?.genWindows?.milkdrop?.open ?? true);
        eqOpenSig.set(state.windows?.genWindows?.equalizer?.open ?? true);
        playlistOpenSig.set(state.windows?.genWindows?.playlist?.open ?? true);
        eqOnSig.set(state.equalizer?.on ?? false);
        eqSlidersSig.set(state.equalizer?.sliders ?? null);
      });
    });

    // Save to localStorage when any signal changes (debounced)
    let saveTimer = /** @type {ReturnType<typeof setTimeout> | null} */ (null);
    let initialized = false;

    effect(() => {
      const snapshot = {
        milkdropOpen: milkdropOpenSig.value,
        eqOpen: eqOpenSig.value,
        playlistOpen: playlistOpenSig.value,
        eqOn: eqOnSig.value,
        eqSliders: eqSlidersSig.value,
      };

      if (!initialized) {
        initialized = true;
        return;
      }

      if (saveTimer != null) clearTimeout(saveTimer);

      saveTimer = setTimeout(() => {
        saveTimer = null;
        saveUiState(snapshot);
      }, 5000);
    });
  }
}

export default WebampElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WebampElement;
export const NAME = "dtw-webamp";

customElements.define(NAME, WebampElement);
