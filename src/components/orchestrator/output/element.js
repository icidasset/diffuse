import { ifDefined } from "lit-html/directives/if-defined.js";
import { DEFAULT_GROUP, DiffuseElement } from "@common/element.js";

import "@components/configurator/output/element.js";
import "@components/transformer/output/refiner/default/element.js";
import "@components/transformer/output/replicator/broadcast/element.js";

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

  // LIFECYCLE

  /**
   * @override
   */
  async connectedCallback() {
    super.connectedCallback();

    /** @type {Set<string>} */
    let previouslyActivated = new Set();

    this.effect(() => {
      const set = this.outputConfigurator.activated();
      const newlyActicated = set.difference(previouslyActivated);

      newlyActicated.forEach((id) => {
        switch (id) {
          case "do-output__dc-output__local": {
            import("@components/output/polymorphic/indexed-db/element.js");
            import("@components/transformer/output/string/json/element.js");
            break;
          }
          case "do-output__dc-output__atproto": {
            import("@components/output/raw/atproto/element.js");
            import(
              "@components/transformer/output/raw/atproto-sync/element.js"
            );
            break;
          }
          case "do-output__dc-output__s3": {
            import("@components/output/bytes/s3/element.js");
            import("@components/transformer/output/bytes/automerge/element.js");
            break;
          }
        }
      });

      previouslyActivated = set;
    });
  }

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

  get activated() {
    return this.outputConfigurator.activated;
  }

  get deselect() {
    return this.outputConfigurator.deselect;
  }

  get options() {
    return this.outputConfigurator.options;
  }

  get select() {
    return this.outputConfigurator.select;
  }

  get selected() {
    return this.outputConfigurator.selected;
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
      ></dop-indexed-db>

      <dob-s3 id="do-output__dob-s3"></dob-s3>
      <dor-atproto id="do-output__dor-atproto"></dor-atproto>

      <!-- OUTPUT CONFIGURATOR -->
      <dc-output
        id="do-output__dc-output"
        default="do-output__dc-output__local"
        group="${ifDefined(group)}"
      >
        <!-- Local -->
        <dtos-json
          id="do-output__dc-output__local"
          output-selector="#do-output__dop-indexed-db__json"
          label="Local"
        ></dtos-json>

        <!-- ATProto -->
        <dtor-atproto-sync
          id="do-output__dc-output__atproto"
          namespace="atproto"
          output-selector="#do-output__dor-atproto"
          label="AT Protocol"
        ></dtor-atproto-sync>

        <!-- S3 -->
        <dtob-automerge
          id="do-output__dc-output__s3"
          namespace="s3"
          output-selector="#do-output__dob-s3"
          label="S3"
        ></dtob-automerge>
      </dc-output>

      <!-- REFINER -->
      <dtor-default
        id="do-output__dtor-default"
        output-selector="#do-output__dc-output"
      ></dtor-default>

      <!-- ENTRY ⬆️ -->
      <dtor-broadcast
        id="do-output__output"
        output-selector="#do-output__dtor-default"
        group="${ifDefined(group)}"
      ></dtor-broadcast>
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
