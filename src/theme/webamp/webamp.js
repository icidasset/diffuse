import Webamp from "webamp/lazy";

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

  connectedCallback() {
    this.attachShadow({ mode: "open" });

    // Custom webamp rendering
    this.renderWebamp();
  }

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
