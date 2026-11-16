import foundation from "~/common/foundation.js";
import {
  AUTOMERGE_NAME,
  CONNECT_URL,
  ENCRYPT_NAME,
  FIRKIN_METHODS,
  FIRKIN_OUTPUT_NAME,
  methodForOption,
  optionId,
  stepId,
  storageId,
} from "~/common/firkin/methods.js";
import { effect } from "~/common/signal.js";

const ATPROTO_OUTPUT_NAME = "dor-atproto-passkey";
const ATPROTO_PASSKEY_NAME = "dtor-passkey-encryption";
const ATPROTO_SYNC_NAME = "dtor-atproto-sync";

const ATPROTO_SPACE_OUTPUT_NAME = "dor-atproto-space";
const ATPROTO_SPACE_SYNC_NAME = "dtor-atproto-space-sync";

const DASL_SYNC_NAME = "dtob-dasl-sync";
const DROPBOX_OUTPUT_NAME = "dob-dropbox";
const S3_OUTPUT_NAME = "dob-s3";

/**
 * Chain modules keyed by tag, so the imports stay static and only run when the
 * method is actually activated.
 *
 * @type {Record<string, () => Promise<unknown>>}
 */
const FIRKIN_CHAIN_MODULES = {
  [ENCRYPT_NAME]: () =>
    import("~/components/transformer/output/bytes/encrypt/element.js"),
  [AUTOMERGE_NAME]: () =>
    import("~/components/transformer/output/bytes/automerge/element.js"),
};

/**
 * @import OutputOrchestrator from "~/components/orchestrator/output/element.js"
 */

/**
 * Marker so it's obvious in the console which build wired this page.
 *
 * @type {string}
 */
const BUILD_MARKER = "output-bundle@carry-over-switch-guard";

/** @type {Set<string>} */
let previouslyActivated = new Set();

/**
 * Setup DOM elements when needed.
 */
effect(() => {
  const output = foundation.signals.orchestrator.output();
  if (!output) return;

  console.info(
    `diffuse: ${BUILD_MARKER} (firkin methods: ${
      FIRKIN_METHODS.map((method) => method.id).join(", ")
    })`,
  );

  atprotoPasskey(output);
  atprotoSpace(output);
  dropbox(output);
  for (const method of FIRKIN_METHODS) firkin(output, method);
  s3(output);

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
    const firkinMethod = methodForOption(id);
    if (firkinMethod) return firkinCustomElements(firkinMethod);

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
  passkey.setAttribute("label", "AT Protocol (Public)");
  passkey.setAttribute("namespace", "atproto-passkey");
  passkey.setAttribute(
    "connect",
    "facets/connect/atproto-passkey/index.tile",
  );
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
  sync.setAttribute("label", "AT Protocol (Space)");
  sync.setAttribute("namespace", "atproto-space");
  sync.setAttribute(
    "connect",
    "facets/connect/atproto-space/index.tile",
  );
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

  const sync = document.createElement(DASL_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__dropbox");
  sync.setAttribute("label", "Dropbox");
  sync.setAttribute("namespace", "dropbox");
  sync.setAttribute(
    "connect",
    "facets/connect/dropbox/index.tile",
  );
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

////////////////////////////////////////////
// FIRKIN
////////////////////////////////////////////

/**
 * Adds the Firkin userdata output, encrypted with the Firkin passkey.
 *
 * ```mermaid
 * flowchart LR
 *   automerge[dtob-automerge] --> encrypt[dtob-encrypt] --> firkin[dob-firkin]
 * ```
 *
 * The Firkin element is an append-only log, so a save appends only the changes
 * since the last push (a few hundred bytes). The encryption element seals each
 * entry, so the instance only ever stores ciphertext. The Automerge transformer
 * turns the log into the five collections and merges concurrent edits per
 * record.
 *
 * @param {OutputOrchestrator} output
 */
/**
 * Adds one Firkin-backed userdata method.
 *
 * The chain is described by the method (see `~/common/firkin/methods.js`):
 * the backing log first, then each transformer with its `output-selector`
 * pointing at the previous element. Only the outermost element is added to the
 * configurator, so a method is one selectable thing.
 *
 * For the current method that is:
 *
 * ```mermaid
 * flowchart LR
 *   automerge[dtob-automerge] --> encrypt[dtob-encrypt] --> firkin[dob-firkin]
 * ```
 *
 * @param {OutputOrchestrator} output
 * @param {import("~/common/firkin/methods.js").FirkinMethod} method
 */
export function firkin(output, method) {
  const storage = document.createElement(FIRKIN_OUTPUT_NAME);
  storage.setAttribute("group", foundation.GROUP);
  storage.setAttribute("id", storageId(method));
  storage.setAttribute("namespace", method.id);
  storage.setAttribute("mode", method.mode);
  storage.setAttribute("keep", String(method.keep));
  output.append(storage);

  let previous = `#${storageId(method)}`;

  for (const name of method.chain) {
    const element = document.createElement(name);
    const isOption = name === method.chain.at(-1);
    const id = isOption ? optionId(method) : stepId(method, name);

    element.setAttribute("group", foundation.GROUP);
    element.setAttribute("id", id);
    element.setAttribute("namespace", method.id);
    element.setAttribute("output-selector", previous);

    if (isOption) {
      element.setAttribute("label", method.label);
      element.setAttribute("connect", CONNECT_URL);
      // The userdata facet shows one row per provider, so methods of the same
      // provider collapse into it. The first method is the one that row
      // activates when nothing is configured yet.
      element.setAttribute("provider", method.provider);
      if (method === FIRKIN_METHODS[0]) {
        element.setAttribute("provider-default", "");
      }
      output.outputConfigurator.append(element);
    } else {
      // Intermediate steps sit in the output root: the configurator lists
      // selectable methods, not the wiring between them.
      output.append(element);
    }

    previous = `#${id}`;
  }
}

/**
 * Import a method's elements and hand the encryption key to the one that needs
 * it, if there is one.
 *
 * @param {import("~/common/firkin/methods.js").FirkinMethod} method
 */
export function firkinCustomElements(method) {
  Promise.all([
    import("~/components/output/bytes/firkin/element.js"),
    ...method.chain.map((name) => FIRKIN_CHAIN_MODULES[name]?.()),
  ])
    .then(async () => {
      if (!method.chain.includes(ENCRYPT_NAME)) return;

      // Signing in derived and stored this key; without a session there is
      // nothing to decrypt, and the output reports itself as not ready.
      const { loadOutputKey } = await import("~/common/firkin/output-key.js");
      const key = await loadOutputKey();
      if (!key) {
        console.warn(
          "Firkin output: no encryption key on this browser yet. Sign in on " +
            "Connect → Firkin to derive one; until then this output cannot " +
            "read or write anything.",
        );
        return;
      }

      const encrypt = document.getElementById(stepId(method, ENCRYPT_NAME));
      if (!encrypt) return;

      /** @type {any} */ (encrypt).setKey(key);
    })
    .catch((err) => {
      console.error("Failed to load the Firkin output:", err);
    });
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

  const sync = document.createElement(DASL_SYNC_NAME);
  sync.setAttribute("group", foundation.GROUP);
  sync.setAttribute("id", "do-output__dc-output__s3");
  sync.setAttribute("label", "S3");
  sync.setAttribute("namespace", "s3");
  sync.setAttribute(
    "connect",
    "facets/connect/s3/index.tile",
  );
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
