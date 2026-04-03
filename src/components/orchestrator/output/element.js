import { ifDefined } from "lit-html/directives/if-defined.js";
import { DEFAULT_GROUP, defineElement, DiffuseElement } from "~/common/element.js";

import "~/components/configurator/output/element.js";
import "~/components/transformer/output/refiner/default/element.js";
import "~/components/transformer/output/refiner/initial-contents/element.js";

import "~/components/output/polymorphic/indexed-db/element.js";
import "~/components/transformer/output/string/json/element.js";

/**
 * @import {RenderArg} from "~/common/element.d.ts"
 * @import {OutputElement} from "~/components/output/types.d.ts"
 * @import {OutputConfiguratorElement} from "~/components/configurator/output/types.d.ts"
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

  // ELEMENT GETTERS

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
    const outputConfigurator = this.root().querySelector(
      "#do-output__dc-output",
    );

    if (!outputConfigurator) {
      throw new Error("Output orchestrator did not render yet.");
    }

    return outputConfigurator;
  }

  // PROXY OUTPUT ACTIONS

  get facets() {
    return this.output.facets;
  }

  get playlistItems() {
    return this.output.playlistItems;
  }

  get settings() {
    return this.output.settings;
  }

  get tracks() {
    return this.output.tracks;
  }

  get ready() {
    return this.output.ready;
  }

  // PROXY ADDITIONAL OUTPUT CONFIGURATOR ACTIONS

  get activated() {
    return this.outputConfigurator.activated.bind(this.outputConfigurator);
  }

  get deselect() {
    return this.outputConfigurator.deselect.bind(this.outputConfigurator);
  }

  get hasDefault() {
    return this.outputConfigurator.hasDefault.bind(this.outputConfigurator);
  }

  get hasSelected() {
    return this.outputConfigurator.hasSelected.bind(this.outputConfigurator);
  }

  get loadSelected() {
    return this.outputConfigurator.loadSelected.bind(this.outputConfigurator);
  }

  get options() {
    return this.outputConfigurator.options.bind(this.outputConfigurator);
  }

  get select() {
    return this.outputConfigurator.select.bind(this.outputConfigurator);
  }

  get selected() {
    return this.outputConfigurator.selected.bind(this.outputConfigurator);
  }

  // RENDER

  /**
   * @param {RenderArg} _
   */
  render({ html }) {
    const group = this.group === DEFAULT_GROUP ? undefined : this.group;

    return html`
      <dop-indexed-db
        id="do-output__dop-indexed-db__json"
        namespace="json"
        group="${ifDefined(group)}"
      ></dop-indexed-db>

      <!-- OUTPUT CONFIGURATOR -->
      <dc-output
        id="do-output__dc-output"
        default="do-output__dc-output__local"
        group="${ifDefined(group)}"
      >
        <dtos-json
          id="do-output__dc-output__local"
          output-selector="#do-output__dop-indexed-db__json"
          label="Local"
        ></dtos-json>
      </dc-output>

      <!-- ENTRY ⬆️ -->
      <dtor-initial-contents
        id="do-output__dtor-initial-contents"
        output-selector="#do-output__dc-output"
      ></dtor-initial-contents>

      <dtor-default
        id="do-output__output"
        output-selector="#do-output__dtor-initial-contents"
        group="${ifDefined(group)}"
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

defineElement(NAME, CLASS);
