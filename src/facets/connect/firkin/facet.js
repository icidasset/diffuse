import * as TID from "@atcute/tid";
import { html, nothing } from "lit-html";

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
import { outputKey } from "~/common/firkin/output-key.js";
import { effect, signal } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup, waitForOutputOption } from "~/facets/connect/common.js";

/**
 * @import {WnfsFirkinOutputElement} from "@specs/components/output/bytes/wnfs/types.d.ts"
 */

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

// The output option comes from an opt-in feature facet, so unlike the bundled
// outputs it may never be registered: the user may simply not have enabled it.
// Wait long enough for a prelude to have run, then carry on without it — this
// page is still useful for signing in and for adding the input.
const OUTPUT_OPTION_WAIT_MS = 2_500;

/** @type {Map<string, { id: string }>} */
const firkinOptions = new Map();

await Promise.all(
  FIRKIN_METHODS.map(async (method) => {
    try {
      firkinOptions.set(
        method.id,
        await waitForOutputOption(
          outputOrchestrator,
          method.label,
          OUTPUT_OPTION_WAIT_MS,
        ),
      );
    } catch {
      // Not an error: the feature facet is disabled, so there is no output to
      // offer. The button is hidden below and the description says why.
    }
  }),
);

/** Whether at least one userdata output option exists, i.e. a feature facet is on. */
const availableMethods = FIRKIN_METHODS.filter((method) =>
  firkinOptions.has(method.id)
);
const canUseOutput = availableMethods.length > 0;

const FIRKIN_OPTION_IDS = new Set(
  [...firkinOptions.values()].map((option) => option.id),
);

/** The WNFS output element, when that optional feature is enabled. */
const WNFS_OUTPUT_NAME = "dob-wnfs-firkin";

/** @returns {WnfsFirkinOutputElement | null} */
function wnfsOutput() {
  const element = outputOrchestrator.root().querySelector(WNFS_OUTPUT_NAME);
  if (!element) return null;

  // The element may exist before its custom-element class is upgraded.
  if (typeof /** @type {any} */ (element).dataRoot !== "function") return null;

  return /** @type {WnfsFirkinOutputElement} */ (
    /** @type {unknown} */ (element)
  );
}

/**
 * The method the dialog is set to. With one available method there is no
 * chooser.
 *
 * @returns {import("~/common/firkin/methods.js").FirkinMethod}
 */
function chosenMethod() {
  const select = document.querySelector("#rb-method");
  const id = select instanceof HTMLSelectElement ? select.value : "";

  return availableMethods.find((method) => method.id === id) ??
    availableMethods[0] ?? FIRKIN_METHODS[0];
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
  outputIcon: "ph-fill ph-scales",
  outputTitle: `Use Firkin as userdata storage`,
  hasOutput: canUseOutput,

  description: html`
    <p>
      Connect to a <a href="https://tangled.org/tokono.ma/firkin" target="_blank">Firkin</a> instance using just a passkey, no email or password.
    </p>
    ${canUseOutput
      ? nothing
      : html`
        <p class="caption">
          Storing your user data here needs the
          <strong>Firkin Output</strong> feature: enable it under Facets →
          Data, then reload this page.
        </p>
      `}
  `,

  formFields: html`
    ${
    availableMethods.length > 1
      ? html`
        <label class="connect-output-only">
          Storage method
          <select id="rb-method">
            ${availableMethods.map((method) =>
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
        value="${initialAccount?.apiBase ?? ""}"
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
 * Sign in when there is no session — and also when there is one but the output
 * key is missing, because the key is only ever derived by the passkey step.
 * Without this a restored session can never recover the key, leaving the output
 * with nothing to read or write.
 *
 * @param {'input' | 'output'} mode
 */
async function ensureAccount(mode) {
  // Adding an audio input only needs the shared account session. The output
  // method select is not part of that flow, so never let it trigger the WNFS
  // setup as a side effect.
  if (mode === "input") {
    if (account.get() && outputKey() !== undefined) return;
    await connectWithDid(mode);
    return;
  }

  const method = chosenMethod();

  // The WNFS output owns its own storage root and exchange key, which only its
  // own `signIn()` creates. A shared account session is not enough to make the
  // output readable.
  if (method.kind === "wnfs") {
    const wnfs = wnfsOutput();
    if (wnfs) {
      await wnfs.whenRestored();
      if (account.get() && outputKey() !== undefined && wnfs.dataRoot()) {
        return;
      }
      await connectWithDid(mode);
      return;
    }
  }

  if (account.get() && outputKey() !== undefined) return;

  await connectWithDid(mode);
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
  await ensureAccount(mode);

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
 *
 * @param {'input' | 'output'} mode
 */
async function connect(mode) {
  const apiBase = apiInput.value.trim() || undefined;
  const method = chosenMethod();

  // Delegate to the WNFS output when it owns the selected method, so it also
  // creates/mounts its encrypted userdata and seeds it from the local output.
  // Inputs only need the shared account, even if WNFS is the stored choice.
  if (mode === "output" && method.kind === "wnfs") {
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
          // The option may not be registered yet; the user can select it.
        }
      }
      return;
    }
  }

  const result = await signInAccount({ apiBase });
  account.value = { did: result.did, apiBase: result.apiBase };

  // The session is valid but the output key is not: say so here, because the
  // output would otherwise appear configured and silently show nothing.
  if (result.keyError) {
    const reason = result.keyError instanceof Error
      ? result.keyError.message
      : String(result.keyError);

    setError(
      "Signed in, but your passkey did not provide the output encryption " +
        `key, so the Firkin output cannot read or write here. (${reason})`,
    );
  }
}

/**
 * Sign in, surfacing the passkey's DID when that is the way forward: an
 * unregistered account needs its DID registered before anything else works.
 *
 * @param {'input' | 'output'} mode
 */
async function connectWithDid(mode) {
  try {
    await connect(mode);
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
      // The WNFS output also clears the shared session; stop using any Firkin
      // output before dropping it.
      const selectedId = outputOrchestrator.selected()?.id;
      if (selectedId !== undefined && FIRKIN_OPTION_IDS.has(selectedId)) {
        await outputOrchestrator.deselect();
      }

      await wnfs.signOut();
    } else {
      // Without a session the output has nothing to read, so stop using it
      // before dropping the account.
      const selectedId = outputOrchestrator.selected()?.id;
      if (selectedId !== undefined && FIRKIN_OPTION_IDS.has(selectedId)) {
        await outputOrchestrator.deselect();
      }

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
