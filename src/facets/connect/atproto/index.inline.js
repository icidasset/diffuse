import "@awesome.me/webawesome/dist/components/input/input.js";

import "~/common/webawesome/detect-dark.js";
import "~/common/webawesome/phosphor/bold.js";

import { html } from "lit-html";

import { NAME as ATPROTO_NAME } from "~/components/output/raw/atproto/element.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

document.title = "Connect AT Protocol | Diffuse";

/**
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 * @import { ATProtoOutputElement } from "~/components/output/raw/atproto/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const outputOrchestrator = await foundation.orchestrator.output();

await customElements.whenDefined(outputOrchestrator.localName);

const atprotoOption = (await outputOrchestrator.options()).find(
  (o) => o.label === "AT Protocol",
);

const atprotoEl = /** @type {ATProtoOutputElement | undefined} */ (
  outputOrchestrator.root().querySelector(ATPROTO_NAME)
);

if (!atprotoOption) {
  throw new Error("AT Protocol output was not enabled!");
}

const ATPROTO_OUTPUT_ID = atprotoOption.id;

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems } = setup({
  title: "AT Protocol",
  hasInput: false,

  description: html`
    <p>
      Connect to your AT Protocol identity to use it as user-data storage.
    </p>
    <p class="wa-caption-xs">
      Your data is stored as lexicon records in your personal data server (PDS).
    </p>
  `,

  formFields: html`
    <wa-input
      id="atproto-handle"
      label="Handle"
      placeholder="you.bsky.social"
      required
    ></wa-input>
    <p class="wa-caption-xs">* Required fields</p>
  `,

  onSubmit: (_mode) => connect(),

  onOutputActivate: async () => {
    await outputOrchestrator.select(ATPROTO_OUTPUT_ID);
  },
});

const handleInput =
  /** @type {WaInput} */ (document.querySelector("#atproto-handle"));

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const did = atprotoEl?.did();
  const isSelectedOutput = outputOrchestrator.selected()?.id === ATPROTO_OUTPUT_ID;

  setItems(
    did
      ? [
          {
            name: did,
            detail: "AT Protocol",
            isInput: false,
            isOutput: true,
            isSelectedOutput,
            onRemove: () => disconnect(),
          },
        ]
      : [],
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

async function connect() {
  const handle = handleInput.value?.trim();
  if (!handle) return;

  await outputOrchestrator.select(ATPROTO_OUTPUT_ID);
  await atprotoEl?.login(handle);
}

async function disconnect() {
  await atprotoEl?.logout();
}
