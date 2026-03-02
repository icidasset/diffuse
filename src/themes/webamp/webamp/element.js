import Webamp from "webamp/lazy";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */
class WebampElement extends HTMLElement {
  constructor() {
    super();

    // ⚡

    /** @type {import("webamp/lazy").default} */
    this.amp = new /** @type {any} */ (Webamp)({
      enableMediaSession: true,
      initialTracks: [],
      zIndex: 99,

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

    return await this.amp.renderWhenReady(ampNode);
  }
}

export default WebampElement;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = WebampElement;
export const NAME = "dtw-webamp";

customElements.define(NAME, WebampElement);
