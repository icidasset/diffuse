import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as DROPBOX_NAME } from "~/components/upload/dropbox/element.js";


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
