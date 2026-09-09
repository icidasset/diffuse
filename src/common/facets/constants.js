/**
 * @example TYPE is the ATProto type string for facets
 * ```js
 * import { TYPE } from "~/common/facets/constants.js";
 *
 * if (TYPE !== "sh.diffuse.output.facet") throw new Error(`expected "sh.diffuse.output.facet", got "${TYPE}"`);
 * ```
 */
export const TYPE = /** @type {const} */ ("sh.diffuse.output.facet");

export const INTERACTIVE = [
  "facets/data/sources/index.tile",
  "facets/data/file-manager/index.tile",
  "facets/data/userdata/index.tile",
  "facets/themes/blur/artwork-controller/facet/index.tile",
  "facets/themes/blur/facet/index.tile",
  "facets/themes/winamp/facet/index.tile",
]

export const PRELUDE_BASE = [
  "facets/data/metadata-bundle/index.tile",
  "facets/data/artwork-bundle/index.tile",
  "facets/data/input-bundle/index.tile",
  "facets/data/output-bundle/index.tile",
  "facets/data/upload-bundle/index.tile",
  "facets/playback/preload/prelude/index.tile",
];

export const PRELUDE_OTHER = [
  "facets/data/process-tracks/prelude/index.tile",
  "facets/misc/scrobble/index.tile",
  "facets/playback/auto-queue/prelude/index.tile",
]

export const STARTING_SET_DISABLED = [
  "facets/misc/scrobble/index.tile",
];

export const STARTING_SET_URIS = [
  ...PRELUDE_BASE,
  ...PRELUDE_OTHER,
  ...INTERACTIVE,
];
