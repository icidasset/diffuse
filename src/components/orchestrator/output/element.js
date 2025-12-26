import { DiffuseElement } from "@common/element.js";

import "@components/configurator/output/element.js";
import "@components/output/polymorphic/indexed-db/element.js";
import "@components/transformer/output/refiner/default/element.js";
import "@components/transformer/output/string/json/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {Track} from "@definitions/types.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

class OutputOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/output";

  /**
   * @returns {OutputElement<Track[]>}
   */
  get output() {
    /** @type {OutputElement<Track[]> | null} */
    const output = this.querySelector("#do-output__output");

    if (!output) throw new Error("Output orchestrator did not render yet.");
    return output;
  }

  // PROXY OUTPUT ACTIONS

  get tracks() {
    return this.output.tracks;
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    return html`
      <dop-indexed-db
        id="do-output__dop-indexed-db__json"
        namespace="json"
      ></dop-indexed-db>

      <dc-output id="do-output__dc-output" default="do-output__dtos-json">
        <dtos-json
          id="do-output__dtos-json"
          output-selector="#do-output__dop-indexed-db__json"
        ></dtos-json>
      </dc-output>

      <dtor-default
        id="do-output__output"
        output-selector="#do-output__dc-output"
      ></dtor-default>
    `;
  }
}

export default OutputOrchestrator;

////////////////////////////////////////////
// REGISTER
////////////////////////////////////////////

export const CLASS = OutputOrchestrator;
export const NAME = "do-output";

customElements.define(NAME, CLASS);
