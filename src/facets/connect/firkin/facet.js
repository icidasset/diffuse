import * as TID from "@atcute/tid";
import { html } from "lit-html";

import {
  DEFAULT_API_BASE,
  passkeyDid as derivePasskeyDid,
  readAccount,
  signInAccount,
  signOutAccount,
  UnregisteredAccountError,
} from "~/common/firkin/account.js";
import { SOURCE_URI } from "~/common/firkin/files.js";
import { FIRKIN_METHODS } from "~/common/firkin/methods.js";
import { effect, signal } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup, waitForOutputOption } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Firkin" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

/** @type {{ did: string; apiBase: string } | null} */
let initialAccount = null;

try {
  const stored = await readAccount();
  if (stored) {
    initialAccount = { did: stored.did, apiBase: stored.apiBase };
  }
} catch {
  // Ignore a corrupt/unavailable local session; the connect form still works.
}

const account = signal(initialAccount);

const outputOrchestrator = await foundation.orchestrator.output();
await customElements.whenDefined(outputOrchestrator.localName);

// Added by the output bundle, so they may not be registered yet on this page.
/** @type {Map<string, { id: string }>} */
const firkinOptions = new Map();

await Promise.all(
  FIRKIN_METHODS.map(async (method) => {
    const option = await waitForOutputOption(outputOrchestrator, method.label);

    firkinOptions.set(method.id, option);
  }),
);

const FIRKIN_OPTION_IDS = new Set(
  [...firkinOptions.values()].map((option) => option.id),
);

/**
 * The method the dialog is set to. With one method there is no chooser.
 *
 * @returns {import("~/common/firkin/methods.js").FirkinMethod}
 */
function chosenMethod() {
  const select = document.querySelector("#rb-method");
  const id = select instanceof HTMLSelectElement ? select.value : "";

  return FIRKIN_METHODS.find((method) => method.id === id) ?? FIRKIN_METHODS[0];
}

/**
 * @param {import("~/common/firkin/methods.js").FirkinMethod} method
 */
function selectMethod(method) {
  const option = firkinOptions.get(method.id);
  if (!option) throw new Error(`${method.label} is not available`);

  return outputOrchestrator.select(option.id);
}

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems, setError, setDialogError } = setup({
  title: "Firkin",
  outputIcon: "ph-fill ph-cloud",
  outputTitle: `Use Firkin as userdata storage`,

  description: html`
    <p>
      Connect to a <a href="https://tangled.org/tokono.ma/firkin" target="_blank">Firkin</a> instance using just a passkey, no email or password.
    </p>
  `,

  formFields: html`
    ${
    FIRKIN_METHODS.length > 1
      ? html`
        <label>
          Storage method
          <select id="rb-method">
            ${FIRKIN_METHODS.map((method) =>
              html`<option value="${method.id}">${method.label}</option>`
            )}
          </select>
        </label>
      `
      : ""
  }
    <label>
      API endpoint
      <input
        id="rb-api"
        placeholder="${DEFAULT_API_BASE}"
        value="${initialAccount?.apiBase ?? DEFAULT_API_BASE}"
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

  onSubmit: (mode) => addFirkin(mode),

  // Already signed in and the output is configured: just make it active.
  onOutputActivate: async () => {
    await selectMethod(chosenMethod());
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

/**
 * Whether one of this account's sources is in the collection. The Firkin input
 * resolves `firkin://me` into `firkin://me/files/...` tracks, so both count.
 */
function hasInputSource() {
  const collection = outputOrchestrator.tracks.collection();
  if (collection.state !== "loaded") return false;

  return collection.data.some((track) =>
    typeof track.uri === "string" &&
    (track.uri === SOURCE_URI || track.uri.startsWith(`${SOURCE_URI}/`))
  );
}

effect(() => {
  const current = account.get();
  const isInput = hasInputSource();
  const selectedId = outputOrchestrator.selected()?.id;
  const isOutput = selectedId !== undefined && FIRKIN_OPTION_IDS.has(selectedId);

  setItems(
    current
      ? [
        {
          name: current.did,
          detail: current.apiBase,
          isInput,
          isOutput,
          isSelectedOutput: isOutput,
          onRemove: () => disconnect(),
        },
      ]
      : [],
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** Refresh the locally displayed account state from IndexedDB. */
async function refreshAccount() {
  try {
    const stored = await readAccount();
    account.value = stored
      ? { did: stored.did, apiBase: stored.apiBase }
      : null;
  } catch {
    account.value = null;
  }
}

/**
 * Add Firkin as an audio source or as the userdata output.
 *
 * Both need the account session, so this signs in first when it has to; the
 * dialog stays open while the passkey prompt runs.
 *
 * @param {'input' | 'output'} mode
 */
async function addFirkin(mode) {
  if (!account.get()) await connectWithDid();

  if (mode === "input") {
    await addInput();
  } else {
    await selectMethod(chosenMethod());
  }
}

/**
 * Add the account's files as an audio source.
 *
 * The input element resolves `firkin://me` into the account's file registry, so
 * a single placeholder track is enough to make them appear.
 */
async function addInput() {
  if (hasInputSource()) return;

  const collection = outputOrchestrator.tracks.collection();
  const existing = collection.state === "loaded" ? collection.data : [];
  const now = new Date().toISOString();

  await outputOrchestrator.tracks.save([
    ...existing,
    {
      $type: "sh.diffuse.output.track",
      id: TID.now(),
      createdAt: now,
      updatedAt: now,
      kind: "placeholder",
      uri: SOURCE_URI,
    },
  ]);
}

/**
 * Sign in: derive the account key from the passkey, mint a one-year session
 * delegation for this browser, and verify that the account is registered.
 */
async function connect() {
  const apiBase = apiInput.value.trim() || undefined;
  const result = await signInAccount({ apiBase });
  account.value = { did: result.did, apiBase: result.apiBase };
}

/**
 * Sign in, surfacing the passkey's DID when that is the way forward: an
 * unregistered account needs its DID registered before anything else works.
 */
async function connectWithDid() {
  try {
    await connect();
  } catch (err) {
    if (err instanceof UnregisteredAccountError) showDid(err.did);
    throw err;
  } finally {
    await refreshAccount();
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
    // Without a session the output has nothing to read, so stop using it
    // before dropping the account.
    const selectedId = outputOrchestrator.selected()?.id;
    if (selectedId !== undefined && FIRKIN_OPTION_IDS.has(selectedId)) {
      await outputOrchestrator.deselect();
    }

    await signOutAccount();

    account.value = null;
  } catch (err) {
    setError(
      err instanceof Error ? err.message : "Failed to disconnect Firkin",
    );
  }
}

/**
 * Derive the DID for an existing passkey, so it can be registered with the
 * Firkin instance before signing in.
 */
showDidBtn?.addEventListener("click", async () => {
  showDidBtn.disabled = true;
  setDialogError(null);
  if (didResult) didResult.hidden = true;

  try {
    showDid(await derivePasskeyDid());
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
      const did = await derivePasskeyDid({ create: true });

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
