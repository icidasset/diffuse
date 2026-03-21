import "@awesome.me/webawesome/dist/components/callout/callout.js";
import "@awesome.me/webawesome/dist/components/input/input.js";

import { html, nothing, render as litRender } from "lit-html";

import { NAME as ATPROTO_NAME } from "~/components/output/raw/atproto/element.js";
import { NAME as PASSKEY_NAME } from "~/components/transformer/output/refiner/track-uri-passkey/element.js";
import { effect, signal } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

document.title = "Connect AT Protocol | Diffuse";

/**
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 * @import { ATProtoOutputElement } from "~/components/output/raw/atproto/types.d.ts"
 * @import TrackUriPasskeyTransformer from "~/components/transformer/output/refiner/track-uri-passkey/element.js"
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

const atprotoPasskeyEl = /** @type {TrackUriPasskeyTransformer | null} */ (
  outputOrchestrator.root().querySelector(
    `${PASSKEY_NAME}[namespace="atproto"]`,
  )
);

if (atprotoPasskeyEl) {
  await customElements.whenDefined(PASSKEY_NAME);
}

const $passkeyError = signal(/** @type {string | null} */ (null));
const $passkeyWorking = signal(false);

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
  const isSelectedOutput =
    outputOrchestrator.selected()?.id === ATPROTO_OUTPUT_ID;

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
// PASSKEY SECTION
////////////////////////////////////////////

if (atprotoPasskeyEl) {
  const passkeyRoot = document.createElement("div");
  passkeyRoot.classList.add("wa-stack");
  document.querySelector("main .card-body")?.appendChild(passkeyRoot);

  effect(() => {
    const passkeyActive = atprotoPasskeyEl.passkeyActive() ?? false;
    const lockedTracksCount = atprotoPasskeyEl.lockedTracks().length ?? 0;
    const passkeyError = $passkeyError.value;
    const passkeyWorking = $passkeyWorking.value;

    litRender(
      html`
        <wa-divider style="margin: var(--spacing) 0"></wa-divider>

        <div>
          <strong>Passkey encryption (optional)</strong>
        </div>

        ${passkeyActive
          ? html`
            <p>Passkey active — Track URIs are encrypted.</p>

            ${passkeyError
              ? html`
                <wa-callout variant="danger">${passkeyError}</wa-callout>
              `
              : nothing}

            <div class="button-row">
              <wa-button
                variant="neutral"
                appearance="outlined"
                @click="${handlePasskeyRemove}"
              >Remove passkey</wa-button>
            </div>

            <p class="wa-caption-xs">
              Removing the passkey will expose all the sensitive information that was
              previously encrypted.
            </p>
          `
          : html`
            <p class="wa-caption-xs">
              Track URIs can optionally be encrypted so that passwords and other sensitive
              authentication details are kept private. Note that, with this enabled, other
              people cannot play audio listed on your account.
            </p>

            ${passkeyError
              ? html`
                <wa-callout variant="danger">${passkeyError}</wa-callout>
              `
              : nothing}

            <div class="button-row">
              <wa-button
                variant="neutral"
                appearance="outlined"
                ?disabled="${passkeyWorking}"
                @click="${handlePasskeySetup}"
              >${passkeyWorking
                ? "Setting up …"
                : "Set up passkey encryption"}</wa-button>
              <wa-button
                variant="neutral"
                appearance="outlined"
                ?disabled="${passkeyWorking}"
                @click="${handlePasskeyAdopt}"
              >${passkeyWorking
                ? "Authenticating …"
                : "Use existing passkey"}</wa-button>
            </div>
          `} ${lockedTracksCount > 0
          ? html`
            <wa-callout variant="warning">
              ${lockedTracksCount} encrypted track(s) cannot be played until you unlock them with
              your passkey.
            </wa-callout>
          `
          : nothing}
      `,
      passkeyRoot,
    );
  });
}

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

async function handlePasskeySetup() {
  if (!atprotoPasskeyEl) return;
  $passkeyError.value = null;
  $passkeyWorking.value = true;
  try {
    await atprotoPasskeyEl.setupPasskey();
  } catch (err) {
    $passkeyError.value = err instanceof Error
      ? err.message
      : "Passkey setup failed";
  } finally {
    $passkeyWorking.value = false;
  }
}

async function handlePasskeyAdopt() {
  if (!atprotoPasskeyEl) return;
  $passkeyError.value = null;
  $passkeyWorking.value = true;
  try {
    await atprotoPasskeyEl.adoptPasskey();
  } catch (err) {
    $passkeyError.value = err instanceof Error
      ? err.message
      : "Passkey adoption failed";
  } finally {
    $passkeyWorking.value = false;
  }
}

async function handlePasskeyRemove() {
  if (!atprotoPasskeyEl) return;
  $passkeyError.value = null;
  await atprotoPasskeyEl.removePasskey();
}
