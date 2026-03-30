import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as AUDIO_METADATA_NAME } from "~/components/artwork/audio-metadata/element.js";
import { NAME as INPUT_NAME } from "~/components/artwork/input/element.js";
import { NAME as LAST_FM_NAME } from "~/components/artwork/last.fm/element.js";
import { NAME as MUSICBRAINZ_NAME } from "~/components/artwork/musicbrainz/element.js";

/**
 * @import ArtworkConfigurator from "~/components/configurator/artwork/element.js"
 * @import InputOrchestrator from "~/components/configurator/input/element.js"
 */

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const artwork = foundation.signals.configurator.artwork();
  const input = foundation.signals.configurator.input();
  if (!artwork || !input) return;

  inputArtwork(artwork, input);
  audioMetadata(artwork, input);
  musicBrainz(artwork);
  lastFm(artwork);
});

////////////////////////////////////////////
// INPUT
////////////////////////////////////////////

/**
 * @param {ArtworkConfigurator} artwork
 * @param {InputOrchestrator} input
 */
export function inputArtwork(artwork, input) {
  const el = document.createElement(INPUT_NAME);
  el.setAttribute("input-selector", input.selector);
  artwork.prepend(el);
}

////////////////////////////////////////////
// AUDIO METADATA
////////////////////////////////////////////

/**
 * @param {ArtworkConfigurator} artwork
 * @param {InputOrchestrator} input
 */
export function audioMetadata(artwork, input) {
  const el = document.createElement(AUDIO_METADATA_NAME);
  el.setAttribute("input-selector", input.selector);
  artwork.append(el);
}

////////////////////////////////////////////
// LAST.FM
////////////////////////////////////////////

/**
 * @param {ArtworkConfigurator} artwork
 */
export function lastFm(artwork) {
  artwork.append(document.createElement(LAST_FM_NAME));
}

////////////////////////////////////////////
// MUSICBRAINZ
////////////////////////////////////////////

/**
 * @param {ArtworkConfigurator} artwork
 */
export function musicBrainz(artwork) {
  artwork.append(document.createElement(MUSICBRAINZ_NAME));
}
