import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as DROPBOX_NAME } from "~/components/upload/dropbox/element.js";
import { NAME as S3_NAME } from "~/components/upload/s3/element.js";

/**
 * @import UploadConfigurator from "~/components/configurator/upload/element.js"
 */

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const upload = foundation.signals.configurator.upload();
  if (!upload) return;

  dropbox(upload);
  s3(upload);
});

////////////////////////////////////////////
// DROPBOX
////////////////////////////////////////////

/**
 * @param {UploadConfigurator} upload
 */
export function dropbox(upload) {
  upload.append(document.createElement(DROPBOX_NAME));
}

/**
 * @param {UploadConfigurator} upload
 */
export function s3(upload) {
  upload.append(document.createElement(S3_NAME));
}
