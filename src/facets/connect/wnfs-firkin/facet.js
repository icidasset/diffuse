import { html, render as litRender } from "lit-html";

import {
  NAME as WNFS_OUTPUT_NAME,
  UnregisteredAccountError,
} from "~/components/output/bytes/wnfs/element.js";
import { DEFAULT_API_BASE } from "~/components/output/bytes/wnfs/constants.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup, waitForOutputOption } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Firkin + WNFS" });

/**
 * @import { WnfsFirkinOutputElement } from "@specs/components/output/bytes/wnfs/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const outputOrchestrator = await foundation.orchestrator.output();
await customElements.whenDefined(outputOrchestrator.localName);
await customElements.whenDefined(WNFS_OUTPUT_NAME);

const wnfsEl = /** @type {WnfsFirkinOutputElement | undefined} */ (
  outputOrchestrator.root().querySelector(WNFS_OUTPUT_NAME)
);

let outputOption;

try {
  outputOption = await waitForOutputOption(
    outputOrchestrator,
    "Firkin + WNFS",
    5_000,
  );
} catch {
  const main = document.querySelector("main");
  if (main) {
    litRender(
      html`
        <div class="facet__left">
          <h1>Firkin + WNFS</h1>
        </div>
        <div class="facet__right">
          <div class="callout callout--danger">
            The <strong>Firkin + WNFS</strong> feature is not enabled.
            Enable it from the
            <a href="./catalogue/?filter=prelude" target="_blank">features page</a> first.
          </div>
        </div>
      `,
      main,
    );
  }

  foundation.ready();
  throw new Error("The Firkin + WNFS feature facet is not enabled");
}

const OUTPUT_ID = outputOption.id;

////////////////////////////////////////////
// RESTORE
////////////////////////////////////////////

// Wait for a previously configured session/WNFS so the list shows the
// account instead of an empty state while the restore is still running.
await wnfsEl?.whenRestored();

// The output configurator can't find custom outputs during its initial
// connectedCallback, so it re-loads the selected output asynchronously.
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

const { setItems, setError, setDialogError } = setup({
  title: "Firkin + WNFS",
  hasInput: false,
  outputLabel: "Set up",
  outputIcon: "ph-fill ph-lightning",
  outputTitle: "Set up Firkin + WNFS",

  description: html`
    <p>
      Store your userdata encrypted on a <a href="https://github.com/wnfs-wg/" target="_blank">Firkin</a> instance using just a passkey (no email or password).
    </p>
  `,

  formFields: html`
    <label>
      API endpoint
      <input
        id="rb-api"
        placeholder="${DEFAULT_API_BASE}"
        value="${wnfsEl?.apiBase() ?? DEFAULT_API_BASE}"
      >
    </label>
    <p class="caption">
      Not registered yet? <strong>Create a new passkey</strong> or
      <strong>Show DID</strong> (for an existing one) to get its DID — the
      instance's admin has to register it first.
    </p>
    <div id="rb-did-result" hidden>
      <p class="caption" style="font-weight: 700; margin-top: 0;">Account DID</p>
      <div class="rb-did-row">
        <code id="rb-did-value"></code>
        <button
          id="rb-copy-did-btn"
          type="button"
          class="button--plain button--icon"
          aria-label="Copy DID"
          title="Copy DID"
        >
          <i class="ph-fill ph-copy"></i>
        </button>
      </div>
      <p class="caption" id="rb-did-hint">
        Register this DID with the instance, then sign in.
      </p>
    </div>
  `,

  footerActions: html`
    <button id="rb-show-did-btn" type="button">
      <i class="ph-fill ph-identification-card"></i>
      Show DID
    </button>
    <button id="rb-create-passkey-btn" type="button">
      <i class="ph-fill ph-fingerprint"></i>
      Create a new passkey
    </button>
  `,

  onSubmit: () => connectWithDid(),

  onOutputActivate: async () => {
    await outputOrchestrator.select(OUTPUT_ID);
  },
});

const dialog =
  /** @type {HTMLDialogElement | null} */ (
    document.querySelector("#connect-dialog")
  );

const apiInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#rb-api"));

const showDidBtn =
  /** @type {HTMLButtonElement | null} */ (
    document.querySelector("#rb-show-did-btn")
  );
const didResult =
  /** @type {HTMLElement | null} */ (document.querySelector("#rb-did-result"));
const didValue =
  /** @type {HTMLElement | null} */ (document.querySelector("#rb-did-value"));
const copyDidBtn =
  /** @type {HTMLButtonElement | null} */ (
    document.querySelector("#rb-copy-did-btn")
  );
const didHint =
  /** @type {HTMLElement | null} */ (document.querySelector("#rb-did-hint"));

const DEFAULT_DID_HINT = "Register this DID with the instance, then sign in.";

// Do not carry a previously shown DID into the next dialog session.
dialog?.addEventListener("close", () => {
  if (didResult) didResult.hidden = true;
  if (didValue) didValue.textContent = "";
  if (didHint) didHint.textContent = DEFAULT_DID_HINT;
});

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const did = wnfsEl?.did();
  const root = wnfsEl?.dataRoot();
  const configured = Boolean(root);
  const isSelectedOutput =
    outputOrchestrator.selected()?.id === OUTPUT_ID;

  setItems(
    did
      ? [
        {
          name: did,
          detail: root
            ? `Firkin + WNFS · ${root.slice(0, 14)}…`
            : "Not set up",
          isInput: false,
          // Without a WNFS root there is no storage yet, so leave the
          // "Use as userdata storage" button able to open the sign-in dialog.
          isOutput: configured,
          isSelectedOutput: configured && isSelectedOutput,
          onRemove: () => disconnect(),
        },
      ]
      : [],
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** Sign in to the instance and make this output the selected one. */
async function connect() {
  if (!wnfsEl) throw new Error("Firkin + WNFS output is not available");

  const apiBase = apiInput.value.trim() || undefined;

  await wnfsEl.signIn({ apiBase });
  await outputOrchestrator.select(OUTPUT_ID);
}

/**
 * Sign in, surfacing the passkey's DID when that is the way forward: an
 * unregistered account needs its DID registered before anything else works.
 */
async function connectWithDid() {
  try {
    await connect();
  } catch (err) {
    const did = err instanceof UnregisteredAccountError
      ? wnfsEl?.lastPasskeyDid()
      : undefined;

    if (did) showDid(did);

    throw err;
  }
}

/**
 * Show a DID in the dialog's result block, ready to copy.
 *
 * @param {string} did
 * @param {string} [hint]
 */
function showDid(did, hint = DEFAULT_DID_HINT) {
  if (didValue) didValue.textContent = did;
  if (didHint) didHint.textContent = hint;
  if (didResult) didResult.hidden = false;
}

async function disconnect() {
  setError(null);
  try {
    await outputOrchestrator.deselect();
    await wnfsEl?.signOut();
  } catch (err) {
    setError(
      err instanceof Error ? err.message : "Failed to disconnect Firkin + WNFS",
    );
  }
}

/**
 * Derive the DID for an existing passkey, so it can be registered with the
 * Firkin instance before signing in.
 */
showDidBtn?.addEventListener("click", async () => {
  if (!wnfsEl) return;

  showDidBtn.disabled = true;
  setDialogError(null);
  if (didResult) didResult.hidden = true;

  try {
    showDid(await wnfsEl.passkeyDid());
  } catch (err) {
    setDialogError(
      err instanceof Error ? err.message : "Could not derive the DID",
    );
  } finally {
    showDidBtn.disabled = false;
  }
});

copyDidBtn?.addEventListener("click", async () => {
  const did = didValue?.textContent?.trim();
  if (!did) return;

  try {
    await navigator.clipboard.writeText(did);

    copyDidBtn.innerHTML = '<i class="ph-fill ph-check"></i>';
    copyDidBtn.setAttribute("aria-label", "Copied");

    setTimeout(() => {
      copyDidBtn.innerHTML = '<i class="ph-fill ph-copy"></i>';
      copyDidBtn.setAttribute("aria-label", "Copy DID");
    }, 1_500);
  } catch {
    setDialogError("Could not copy the DID to the clipboard");
  }
});

document
  .querySelector("#rb-create-passkey-btn")
  ?.addEventListener("click", async (event) => {
    const button = /** @type {HTMLButtonElement} */ (event.currentTarget);

    button.disabled = true;
    setDialogError(null);

    try {
      if (!wnfsEl) throw new Error("Firkin + WNFS output is not available");

      // Deliberately local: the account cannot be used until an admin
      // registers the DID it derives, so there is nothing to ask the
      // instance yet. Signing in is a separate step, once it is registered.
      const did = await wnfsEl.passkeyDid({ create: true });

      showDid(
        did,
        "Passkey created. Register this DID with the instance, then sign in.",
      );
    } catch (err) {
      setDialogError(
        err instanceof Error ? err.message : "Passkey setup failed",
      );
    } finally {
      button.disabled = false;
    }
  });

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
