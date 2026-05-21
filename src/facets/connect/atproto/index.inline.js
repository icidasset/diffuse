import { html, nothing, render as litRender } from "lit-html";

import { NAME as ATPROTO_NAME } from "~/components/output/raw/atproto/element.js";
import { NAME as PASSKEY_NAME } from "~/components/transformer/output/refiner/passkey-encryption/element.js";
import { effect, signal } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Atmosphere | Diffuse" });

/**
 * @import { ATProtoOutputElement } from "@specs/components/output/raw/atproto/types.d.ts"
 * @import PasskeyEncryptionTransformer from "~/components/transformer/output/refiner/passkey-encryption/element.js"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const outputOrchestrator = await foundation.orchestrator.output();

await customElements.whenDefined(outputOrchestrator.localName);
await customElements.whenDefined(ATPROTO_NAME);

const atprotoOption = await outputOrchestrator.waitForOption("AT Protocol");
const ATPROTO_OUTPUT_ID = atprotoOption.id;

const atprotoEl = /** @type {ATProtoOutputElement | undefined} */ (
  outputOrchestrator.root().querySelector(ATPROTO_NAME)
);

const atprotoPasskeyEl = /** @type {PasskeyEncryptionTransformer | null} */ (
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

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems } = setup({
  title: "Atmosphere",
  hasInput: false,

  description: html`
    <p>
      Use your AT Protocol identity to store your Diffuse user-data.
    </p>
  `,

  formFields: html`
    <label>Handle <input id="atproto-handle" placeholder="you.bsky.social" required></label>
  `,

  onSubmit: (_mode) => connect(),

  onOutputActivate: async () => {
    await outputOrchestrator.select(ATPROTO_OUTPUT_ID);
  },
});

const handleInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#atproto-handle"));

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
  document.querySelector("main .facet__right")?.appendChild(passkeyRoot);

  effect(() => {
    const passkeyActive = atprotoPasskeyEl.passkeyActive() ?? false;
    const lockedTracksCount = atprotoPasskeyEl.lockedTracks().length ?? 0;
    const passkeyError = $passkeyError.value;
    const passkeyWorking = $passkeyWorking.value;

    litRender(
      html`
        <hr>

        <div>
          <strong>Passkey encryption</strong>
        </div>

        ${passkeyActive
          ? html`
            <p>Passkey is configured. Track URIs and settings are encrypted.</p>

            ${passkeyError
              ? html`<div class="callout callout--danger">${passkeyError}</div>`
              : nothing}

            <div class="button-row">
              <button @click="${handlePasskeyRemove}">Remove passkey</button>
            </div>

            <p class="caption">
              Removing the passkey will expose all the sensitive information that was previously encrypted.
            </p>
          `
          : html`
            <p class="caption">
              Track URIs and settings can optionally be encrypted so that passwords and other sensitive details are kept private.
            </p>

            ${passkeyError
              ? html`<div class="callout callout--danger">${passkeyError}</div>`
              : nothing}

            <div class="button-row">
              <button ?disabled="${passkeyWorking}" @click="${handlePasskeySetup}">
                ${passkeyWorking ? "Setting up …" : "Set up passkey encryption"}
              </button>
              <button ?disabled="${passkeyWorking}" @click="${handlePasskeyAdopt}">
                ${passkeyWorking ? "Authenticating …" : "Use existing passkey"}
              </button>
            </div>
          `}
        ${lockedTracksCount > 0
          ? html`
            <div class="callout callout--warning">
              ${lockedTracksCount} encrypted track(s) cannot be played until you unlock them with
              your passkey.
            </div>
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
  await outputOrchestrator.deselect();
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

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
