import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

import { NAME as ATPROTO_OUTPUT_NAME } from "~/components/output/raw/atproto-passkey/element.js";
import { NAME as ATPROTO_SYNC_NAME } from "~/components/transformer/output/raw/atproto-sync/element.js";
import { NAME as ATPROTO_PASSKEY_NAME } from "~/components/transformer/output/refiner/passkey-encryption/element.js";

import { NAME as ATPROTO_SPACE_OUTPUT_NAME } from "~/components/output/raw/atproto-space/element.js";
import { NAME as ATPROTO_SPACE_SYNC_NAME } from "~/components/transformer/output/raw/atproto-space-sync/element.js";

import { NAME as S3_OUTPUT_NAME } from "~/components/output/bytes/s3/element.js";
import { NAME as DROPBOX_OUTPUT_NAME } from "~/components/output/bytes/dropbox/element.js";
import { NAME as S3_SYNC_NAME } from "~/components/transformer/output/bytes/dasl-sync/element.js";


/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

/** @type {Set<string>} */
let previouslyActivated = new Set();

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  atprotoPasskey(output);
  atprotoSpace(output);
  s3(output);
  dropbox(output);

  output.loadSelected();
});

/**
 * Dynamically import custom elements when needed.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  const set = output.activated();
  const newlyActicated = set.difference(previouslyActivated);

  previouslyActivated = set;

  Array.from(newlyActicated).map((id) => {
    switch (id) {
      case "do-output__dc-output__atproto-passkey":
        return atprotoPasskeyCustomElements();
      case "do-output__dc-output__atproto-space":
        return atprotoSpaceCustomElements();
      case "do-output__dc-output__s3":
        return s3CustomElements();
      case "do-output__dc-output__dropbox":
        return dropboxCustomElements();
    }
  });
});

////////////////////////////////////////////
// AT PROTOCOL
////////////////////////////////////////////

/**
 * Adds a few extra layers of functionality to the AT Protocol output,
 * optional passkey encryption and local-first syncing.
 *
 * @param {OutputOrchestrator} output
 */
export function atprotoPasskey(output) {
  const out = document.createElement(ATPROTO_OUTPUT_NAME);
  out.setAttribute("group", foundation.GROUP);
  out.setAttribute("id", "do-output__dor-atproto-passkey");

  const sync = document.createElement(ATPROTO_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dtor-atproto-sync");
  sync.setAttribute("namespace", "atproto-passkey");
  sync.setAttribute("output-selector", "#do-output__dor-atproto-passkey");

  const passkey = document.createElement(ATPROTO_PASSKEY_NAME);
  passkey.setAttribute("group", foundation.GROUP);
  passkey.setAttribute("id", "do-output__dc-output__atproto-passkey");
  passkey.setAttribute("label", "AT Protocol");
  passkey.setAttribute("namespace", "atproto-passkey");
  passkey.setAttribute(
    "output-selector",
    "#do-output__dtor-atproto-sync",
  );

  // Add elements to DOM, make sure the transformer
  // is added to the configurator so the user can select it.
  output.append(out);
  output.append(sync);
  output.outputConfigurator.append(passkey);
}

export function atprotoPasskeyCustomElements() {
  import("~/components/output/raw/atproto-passkey/element.js");
  import(
    "~/components/transformer/output/raw/atproto-sync/element.js"
  );
  import(
    "~/components/transformer/output/refiner/passkey-encryption/element.js"
  );
}

////////////////////////////////////////////
// AT PROTOCOL (SPACES)
////////////////////////////////////////////

/**
 * Exposes the AT Protocol space output as a user-data storage option.
 *
 * Unlike the public AT Protocol output there is no firehose-based sync or
 * passkey encryption. The raw space output is wrapped with a local-first sync
 * transformer (IndexedDB cache + union merge) so data — including the default
 * facets — is persisted and synced with the space.
 *
 * @param {OutputOrchestrator} output
 */
export function atprotoSpace(output) {
  const out = document.createElement(ATPROTO_SPACE_OUTPUT_NAME);
  out.setAttribute("group", foundation.GROUP);
  out.setAttribute("id", "do-output__dor-atproto-space");

  const sync = document.createElement(ATPROTO_SPACE_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__atproto-space");
  sync.setAttribute("label", "AT Protocol Space");
  sync.setAttribute("namespace", "atproto-space");
  sync.setAttribute("output-selector", "#do-output__dor-atproto-space");

  output.append(out);
  output.outputConfigurator.append(sync);
}

export function atprotoSpaceCustomElements() {
  import("~/components/output/raw/atproto-space/element.js");
  import(
    "~/components/transformer/output/raw/atproto-space-sync/element.js"
  );
}

////////////////////////////////////////////
// S3
////////////////////////////////////////////

/**
 * Wraps user data in a custom CBOR encode data structure
 * which tracks what identifiers are removed.
 * The transformer used here allows for merging of data.
 *
 * @param {OutputOrchestrator} output
 */
export function s3(output) {
  const out = document.createElement(S3_OUTPUT_NAME);
  out.setAttribute("group", foundation.GROUP);
  out.setAttribute("id", "do-output__dob-s3");

  const sync = document.createElement(S3_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__s3");
  sync.setAttribute("label", "S3");
  sync.setAttribute("namespace", "s3");
  sync.setAttribute("output-selector", "#do-output__dob-s3");

  // Add elements to DOM, make sure the transformer
  // is added to the configurator so the user can select it.
  output.append(out);
  output.outputConfigurator.append(sync);
}

export function s3CustomElements() {
  import("~/components/output/bytes/s3/element.js");
  import(
    "~/components/transformer/output/bytes/dasl-sync/element.js"
  );
}

////////////////////////////////////////////
// DROPBOX
////////////////////////////////////////////

/**
 * Wraps user data in a custom CBOR encode data structure
 * which tracks what identifiers are removed.
 * The transformer used here allows for merging of data.
 *
 * @param {OutputOrchestrator} output
 */
export function dropbox(output) {
  const out = document.createElement(DROPBOX_OUTPUT_NAME);
  out.setAttribute("group", foundation.GROUP);
  out.setAttribute("id", "do-output__dob-dropbox");

  const sync = document.createElement(S3_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__dropbox");
  sync.setAttribute("label", "Dropbox");
  sync.setAttribute("namespace", "dropbox");
  sync.setAttribute("output-selector", "#do-output__dob-dropbox");

  // Add elements to DOM, make sure the transformer
  // is added to the configurator so the user can select it.
  output.append(out);
  output.outputConfigurator.append(sync);
}

export function dropboxCustomElements() {
  import("~/components/output/bytes/dropbox/element.js");
  import(
    "~/components/transformer/output/bytes/dasl-sync/element.js"
  );
}
