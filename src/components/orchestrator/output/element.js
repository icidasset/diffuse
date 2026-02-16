import { ifDefined } from "lit-html/directives/if-defined.js";
import { DEFAULT_GROUP, DiffuseElement } from "@common/element.js";

import "@components/configurator/output/element.js";
import "@components/configurator/output-fallback/element.js";
import "@components/output/polymorphic/indexed-db/element.js";
import "@components/output/raw/atproto/element.js";
// import "@components/output/bytes/automerge-repo-server/element.js";
// import "@components/transformer/output/bytes/automerge/element.js";
import "@components/transformer/output/refiner/default/element.js";
import "@components/transformer/output/string/json/element.js";

/**
 * @import {RenderArg} from "@common/element.d.ts"
 * @import {OutputElement} from "@components/output/types.d.ts"
 * @import {OutputConfiguratorElement} from "@components/configurator/output/types.d.ts"
 */

////////////////////////////////////////////
// ELEMENT
////////////////////////////////////////////

/**
 * A default setup for managing output.
 *
 * @implements {OutputConfiguratorElement}
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

  /**
   * @returns {OutputConfiguratorElement}
   */
  get outputConfigurator() {
    /** @type {OutputConfiguratorElement | null} */
    const outputConfigurator = this.root().querySelector("#do-output__dc-output");

    if (!outputConfigurator) throw new Error("Output orchestrator did not render yet.");
    return outputConfigurator;
  }

  // PROXY OUTPUT ACTIONS

  get facets() {
    return this.output.facets;
  }

  get playlists() {
    return this.output.playlists;
  }

  get themes() {
    return this.output.themes;
  }

  get tracks() {
    return this.output.tracks;
  }

  get ready() {
    return this.output.ready;
  }

  // PROXY ADDITIONAL OUTPUT CONFIGURATOR ACTIONS

  get deselect() {
    return this.outputConfigurator.deselect
  }

  get options() {
    return this.outputConfigurator.options
  }

  get select() {
    return this.outputConfigurator.select
  }

  get selectedOutput() {
    return this.outputConfigurator.selectedOutput
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const group = this.group === DEFAULT_GROUP ? undefined : this.group;

    return html`
      <!--<dob-automerge-repo-server
        id="do-output__dob-automerge-repo-server"
        namespace="automerge-repo-server"
        url="http://localhost:3030"
      ></dob-automerge-repo-server>-->

      <dop-indexed-db
        id="do-output__dop-indexed-db__json"
        group="${ifDefined(group)}"
        namespace="json"
      ></dop-indexed-db>

      <dc-output id="do-output__dc-output" default="do-output__dtos-json">
        <dtos-json
          id="do-output__dtos-json"
          label="IndexedDB as a JSON string"
          output-selector="#do-output__dop-indexed-db__json"
        ></dtos-json>

        <dc-output-fallback
          id="do-output__dor-atproto-fallback"
          label="AT Protocol"
        >
          <dor-atproto
            id="do-output__dor-atproto"
            group="${ifDefined(group)}"
          ></dor-atproto>
          <dop-indexed-db
            id="do-output__dop-indexed-db__atproto"
            group="${ifDefined(group)}"
            namespace="atproto"
          ></dop-indexed-db>
        </dc-output-fallback>

        <!--<dor-automerge-repo
          id="do-output__dor-automerge-repo"
          namespace="automerge-repo"
        ></dor-automerge-repo>-->

        <!--<dtob-automerge
          id="do-output__dtob-automerge"
          output-selector="#do-output__dob-automerge-repo-server"
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
