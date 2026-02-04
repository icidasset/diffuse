import { ifDefined } from "lit-html/directives/if-defined.js";
import { DEFAULT_GROUP, DiffuseElement } from "@common/element.js";

import "@components/configurator/output/element.js";
import "@components/output/polymorphic/indexed-db/element.js";
// import "@components/transformer/output/bytes/automerge/element.js";
import "@components/transformer/output/refiner/default/element.js";
import "@components/transformer/output/string/json/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * A default setup for managing output.
 */
class OutputOrchestrator extends DiffuseElement {
  static NAME = "diffuse/orchestrator/output";

  /**
   * @returns {OutputElement}
   */
  get output() {
    /** @type {OutputElement | null} */
    const output = this.root().querySelector("#do-output__output");

    if (!output) throw new Error("Output orchestrator did not render yet.");
    return output;
  }

  // PROXY OUTPUT ACTIONS

  get constituents() {
    return this.output.constituents;
  }

  get tracks() {
    return this.output.tracks;
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const group = this.group === DEFAULT_GROUP ? undefined : this.group;

    return html`
      <!--<dop-indexed-db
        id="do-output__dop-indexed-db__bytes--automerge"
        group="${ifDefined(group)}"
        namespace="bytes/automerge"
      ></dop-indexed-db>-->

      <dop-indexed-db
        id="do-output__dop-indexed-db__json"
        group="${ifDefined(group)}"
        namespace="json"
      ></dop-indexed-db>

      <dc-output id="do-output__dc-output" default="do-output__dtos-json">
        <dtos-json
          id="do-output__dtos-json"
          output-selector="#do-output__dop-indexed-db__json"
        ></dtos-json>

        <!--<dtob-automerge
          id="do-output__dtob-automerge"
          output-selector="#do-output__dop-indexed-db__bytes--automerge"
        ></dtob-automerge>-->
      </dc-output>

      <!-- Entry -->
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
