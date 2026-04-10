/**
 * @example TYPE is the ATProto type string for facets
 * ```js
 * import { TYPE } from "~/common/facets/constants.js";
 *
 * if (TYPE !== "sh.diffuse.output.facet") throw new Error(`expected "sh.diffuse.output.facet", got "${TYPE}"`);
 * ```
 */
export const TYPE = /** @type {const} */ ("sh.diffuse.output.facet");

export const STARTING_SET_DISABLED = [
  "facets/misc/scrobble/index.html",
];

export const STARTING_SET_URIS = [
  // INTERACTIVE
  "facets/connect/index.html",
  "themes/blur/artwork-controller/facet/index.html",

  // PRELUDES
  "facets/data/metadata-bundle/index.html",
  "facets/data/artwork-bundle/index.html",
  "facets/data/input-bundle/index.html",
  "facets/data/output-bundle/index.html",
  "facets/data/process-tracks/prelude/index.html",
  "facets/misc/scrobble/index.html",
  "facets/playback/auto-queue/prelude/index.html",
  "facets/playback/preload/prelude/index.html",
];
