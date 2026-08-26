import { html, render as litRender } from "lit-html";

import { NAME as ATPROTO_SPACE_NAME } from "~/components/output/raw/atproto-space/element.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup, waitForOutputOption } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Atmosphere Spaces | Diffuse" });

/**
 * @import { ATProtoSpaceOutputElement } from "@specs/components/output/raw/atproto-space/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const outputOrchestrator = await foundation.orchestrator.output();

await customElements.whenDefined(outputOrchestrator.localName);
await customElements.whenDefined(ATPROTO_SPACE_NAME);

const atprotoOption = await waitForOutputOption(outputOrchestrator, "AT Protocol (Space)");
const ATPROTO_SPACE_OUTPUT_ID = atprotoOption.id;

const atprotoEl = /** @type {ATProtoSpaceOutputElement | undefined} */ (
  outputOrchestrator.root().querySelector(ATPROTO_SPACE_NAME)
);

////////////////////////////////////////////
// OAUTH CALLBACK LOADING STATE
////////////////////////////////////////////

if (true) {
  litRender(
    html`
      <div class="facet__left"></div>
      <div class="facet__right">
        <p class="with-icon">
          <i class="ph-bold ph-spinner animate-spin"></i>
          Connecting to the Atmosphere
        </p>
      </div>
    `,
    /** @type {HTMLElement} */ (document.querySelector("main")),
  );

  await atprotoEl?.whenRestored();
}

// The output configurator can't find custom outputs (added by the
// output-bundle prelude) during its initial connectedCallback, so it
// re-loads the selected output via `loadSelected()` which is async.
if (outputOrchestrator.hasSelected() && !outputOrchestrator.selected()) {
  await new Promise((resolve) => {
    const stop = effect(() => {
      if (outputOrchestrator.selected()) {
        stop();
        resolve(undefined);
      }
    });
  });
}

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems } = setup({
  title: "Atmosphere (Space)",
  hasInput: false,

  description: html`
    <p>
      Use your AT Protocol account for user-data storage. Your data is stored in your own repo and is not broadcast publicly.
    </p>
  `,

  formFields: html`
    <label>Handle <input id="atproto-space-handle" placeholder="you.bsky.social" required></label>
  `,

  onSubmit: (_mode) => connect(),

  onOutputActivate: async () => {
    await outputOrchestrator.select(ATPROTO_SPACE_OUTPUT_ID);
  },
});

const handleInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#atproto-space-handle"));

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const did = atprotoEl?.did();
  const isSelectedOutput =
    outputOrchestrator.selected()?.id === ATPROTO_SPACE_OUTPUT_ID;

  setItems(
    did
      ? [
        {
          name: did,
          detail: "AT Protocol space",
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

  await outputOrchestrator.select(ATPROTO_SPACE_OUTPUT_ID);
  await atprotoEl?.login(handle);
}

async function disconnect() {
  await outputOrchestrator.deselect();
  await atprotoEl?.logout();
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
