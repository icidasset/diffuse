import { DiffuseElement, query, whenElementsDefined } from "@common/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 * @import {InputElement} from "@components/input/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 */

class Browser extends DiffuseElement {
  constructor() {
    super();
    this.attachShadow({ mode: "open" });
  }

  // LIFECYCLE

  /**
   * @override
   */
  connectedCallback() {
    super.connectedCallback();

    /** @type {InputElement} */
    this.input = query(this, "input-selector");

    /** @type {OutputElement<Track[]>} */
    this.output = query(this, "output-selector");

    /** @type {import("@components/engine/queue/element.js").CLASS} */
    this.queue = query(this, "queue-engine-selector");

    // Wait for the above dependencies to be defined, then render again.
    whenElementsDefined({ input: this.input, output: this.output }).then(() => {
      this.effect(() => {
        this.forceRender();
      });
    });
  }

  // EVENTS

  /**
   * @param {MouseEvent} event
   */
  highlightTableEntry(event) {
    if (event.target instanceof HTMLElement === false) return;

    const tr = event.target.tagName === "TR"
      ? event.target
      : event.target.closest("tr");
    if (!tr) return;

    tr.parentElement?.querySelector("tr.highlighted")?.classList.remove(
      "highlighted",
    );
    tr.classList.add("highlighted");
  }

  /**
   * @param {Track} track
   */
  playTrack(track) {
    this.queue?.add({
      inFront: true,
      tracks: [track],
    });
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const tracks = this.output?.tracks?.collection() ?? [];

    return html`
      <link rel="stylesheet" href="../../styles/vendor/98.css" />
      <link rel="stylesheet" href="./98-vars.css" />

      <style>
      /***********************************
      * SEARCH
      ***********************************/

      search {
        margin-bottom: var(--grouped-button-spacing);
      }

      search input {
        flex: 1;
      }

      /***********************************
      * TABLE
      ***********************************/

      .sunken-panel {
        height: 30dvh;
        min-height: 80px;
        resize: both;
      }

      table {
        color: var(--text-color);
        table-layout: fixed;
        width: 100%;
      }

      table th {
        width: 30%;

        &:first-child {
          width: 40%;
        }
      }

      table tbody tr {
        content-visibility: auto;
      }

      table td {
        contain-intrinsic-size: auto 14px;
        overflow: hidden;
        text-overflow: ellipsis;
      }
      </style>

      <search class="field-row">
        <label for="search-input">Search</label>
        <input id="search-input" type="search" />
      </search>

      <div class="sunken-panel" style="width: 480px">
        <table>
          <thead>
            <tr>
              <th>Title</th>
              <th>Artist</th>
              <th>Album</th>
            </tr>
          </thead>
          <tbody>
            ${tracks.map((track) => {
              return html`
                <tr @click="${this.highlightTableEntry}" @dblclick="${() =>
                  this.playTrack(track)}">
                  <td>${track.tags?.title}</td>
                  <td>${track.tags?.artist}</td>
                  <td>${track.tags?.album}</td>
                </tr>
              `;
            })}
          </tbody>
        </table>
      </div>
    `;
  }
}

export default Browser;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = Browser;
export const NAME = "dtw-browser";

customElements.define(NAME, Browser);
