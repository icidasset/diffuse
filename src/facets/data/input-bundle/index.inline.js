import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as DROPBOX_NAME } from "~/components/input/dropbox/element.js";
import { NAME as EPHEMERAL_CACHE_NAME } from "~/components/input/ephemeral-cache/element.js";
import { NAME as HTTPS_NAME } from "~/components/input/https/element.js";
import { NAME as ICECAST_NAME } from "~/components/input/icecast/element.js";
import { NAME as LOCAL_NAME } from "~/components/input/local/element.js";
import { NAME as OPENSUBSONIC_NAME } from "~/components/input/opensubsonic/element.js";
import { NAME as S3_NAME } from "~/components/input/s3/element.js";
import { NAME as WEBDAV_NAME } from "~/components/input/webdav/element.js";


/**
 * @import InputConfigurator from "~/components/configurator/input/element.js"
 */

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const input = foundation.signals.configurator.input();
  if (!input) return;

  dropbox(input);
  ephemeralCache(input);
  https(input);
  icecast(input);
  local(input);
  opensubsonic(input);
  s3(input);
  webdav(input);
});

////////////////////////////////////////////
// DROPBOX
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function dropbox(input) {
  input.append(document.createElement(DROPBOX_NAME));
}

////////////////////////////////////////////
// EPHEMERAL CACHE
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function ephemeralCache(input) {
  input.append(document.createElement(EPHEMERAL_CACHE_NAME));
}

////////////////////////////////////////////
// HTTPS
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function https(input) {
  input.append(document.createElement(HTTPS_NAME));
}

////////////////////////////////////////////
// ICECAST
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function icecast(input) {
  input.append(document.createElement(ICECAST_NAME));
}

////////////////////////////////////////////
// LOCAL
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function local(input) {
  input.append(document.createElement(LOCAL_NAME));
}

////////////////////////////////////////////
// OPENSUBSONIC
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function opensubsonic(input) {
  input.append(document.createElement(OPENSUBSONIC_NAME));
}

////////////////////////////////////////////
// S3
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function s3(input) {
  input.append(document.createElement(S3_NAME));
}

////////////////////////////////////////////
// WEBDAV
////////////////////////////////////////////

/**
 * @param {InputConfigurator} input
 */
export function webdav(input) {
  input.append(document.createElement(WEBDAV_NAME));
}
