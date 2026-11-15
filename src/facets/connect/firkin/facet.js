import { html } from "lit-html";

import {
  DEFAULT_API_BASE,
  passkeyDid as derivePasskeyDid,
  readAccount,
  signInAccount,
  signOutAccount,
  UnregisteredAccountError,
} from "~/common/firkin/account.js";
import { effect, signal } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Firkin" });

/**
 * The WNFS output is optional: when the Firkin + WNFS feature is enabled its
 * element is present, and this page sets it up as well. When it is not, this
 * page only signs in the shared Firkin account (input + uploader).
 *
 * @typedef {HTMLElement & {
 *   signIn(options?: { apiBase?: string; create?: boolean }): Promise<{ did: string; root: string; created: boolean }>;
 *   signOut(): Promise<void>;
 *   passkeyDid(options?: { create?: boolean }): Promise<string>;
 *   apiBase(): string | undefined;
 *   dataRoot(): string | undefined;
 * }} WnfsOutputLike
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const WNFS_OUTPUT_NAME = "dob-wnfs-firkin";

const outputOrchestrator = await foundation.orchestrator.output();
await customElements.whenDefined(outputOrchestrator.localName);

/** @returns {WnfsOutputLike | null} */
function wnfsOutput() {
  const element = outputOrchestrator.root().querySelector(WNFS_OUTPUT_NAME);
  if (!element) return null;

  // The element may exist before its custom-element class is upgraded.
  if (typeof /** @type {any} */ (element).dataRoot !== "function") {
    return null;
  }

  return /** @type {WnfsOutputLike} */ (/** @type {unknown} */ (element));
}

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

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems, setError, setDialogError } = setup({
  title: "Firkin",
  hasInput: false,
  outputLabel: "Connect",
  outputIcon: "ph-fill ph-fingerprint",
  outputTitle: "Connect Firkin",

  description: html`
    <p>
      Connect to a <a href="https://tangled.org/tokono.ma/firkin" target="_blank">Firkin</a> instance using just a passkey (no email or password).
    </p>
    <p class="caption">
      This account powers the Firkin audio input and uploader.
    </p>
  `,

  formFields: html`
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

  onSubmit: () => connectWithDid(),

  onOutputActivate: async () => {
    const wnfs = wnfsOutput();
    if (wnfs?.id) await outputOrchestrator.select(wnfs.id);
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
  const current = account.get();
  const wnfs = wnfsOutput();
  const root = wnfs?.dataRoot();
  const isSelectedOutput = wnfs
    ? outputOrchestrator.selected()?.id === wnfs.id
    : false;

  setItems(
    current
      ? [
        {
          name: current.did,
          detail: root
            ? `Firkin + WNFS · ${root.slice(0, 14)}…`
            : current.apiBase,
          isInput: false,
          // The WNFS output badge is only shown when that optional feature is
          // present and has storage.
          isOutput: Boolean(wnfs && root),
          isSelectedOutput: Boolean(wnfs && root && isSelectedOutput),
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
 * Sign in. When the WNFS output is enabled, delegate to it so it also sets up
 * its encrypted userdata; otherwise use the shared account sign-in only.
 */
async function connect() {
  const apiBase = apiInput.value.trim() || undefined;
  const wnfs = wnfsOutput();

  if (wnfs) {
    const result = await wnfs.signIn({ apiBase });
    account.value = {
      did: result.did,
      apiBase: wnfs.apiBase() ?? apiBase ?? DEFAULT_API_BASE,
    };

    if (wnfs.id && !outputOrchestrator.hasSelected()) {
      try {
        await outputOrchestrator.select(wnfs.id);
      } catch {
        // The output option may not be registered yet; the user can select it.
      }
    }
    return;
  }

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
    const wnfs = wnfsOutput();

    if (wnfs) {
      if (outputOrchestrator.selected()?.id === wnfs.id) {
        await outputOrchestrator.deselect();
      }
      await wnfs.signOut();
    } else {
      await signOutAccount();
    }

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
    const wnfs = wnfsOutput();
    showDid(
      wnfs ? await wnfs.passkeyDid() : await derivePasskeyDid(),
    );
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
      const wnfs = wnfsOutput();
      const did = wnfs
        ? await wnfs.passkeyDid({ create: true })
        : await derivePasskeyDid({ create: true });

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
