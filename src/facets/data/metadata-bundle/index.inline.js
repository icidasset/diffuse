import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as AUDIO_FILE_NAME } from "~/components/metadata/audio-file/element.js";

/**
 * @import MetadataConfigurator from "~/components/configurator/metadata/element.js"
 * @import InputOrchestrator from "~/components/configurator/input/element.js"
 */

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const metadata = foundation.signals.configurator.metadata();
  const input = foundation.signals.configurator.input();
  if (!metadata || !input) return;

  audioFile(metadata, input);
});

////////////////////////////////////////////
// AUDIO FILE
////////////////////////////////////////////

/**
 * @param {MetadataConfigurator} metadata
 * @param {InputOrchestrator} input
 */
export function audioFile(metadata, input) {
  const el = document.createElement(AUDIO_FILE_NAME);
  el.setAttribute("input-selector", input.selector);
  metadata.append(el);
}
