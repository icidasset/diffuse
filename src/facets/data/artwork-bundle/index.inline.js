import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as AUDIO_METADATA_NAME } from "~/components/artwork/audio-metadata/element.js";
import { NAME as LAST_FM_NAME } from "~/components/artwork/last.fm/element.js";
import { NAME as MUSICBRAINZ_NAME } from "~/components/artwork/musicbrainz/element.js";

/**
 * @import ArtworkConfigurator from "~/components/configurator/artwork/element.js"
 */

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const artwork = foundation.signals.configurator.artwork();
  const input = foundation.signals.configurator.input();
  if (!artwork || !input) return;

  audioMetadata(artwork, input);
  lastFm(artwork);
  musicBrainz(artwork);
});

////////////////////////////////////////////
// AUDIO METADATA
////////////////////////////////////////////

/**
 * @param {ArtworkConfigurator} artwork
 * @param {import("~/components/configurator/input/element.js").default} input
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
