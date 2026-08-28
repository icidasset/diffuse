import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/dropbox/constants.js";
import {
  buildURI,
  parseURI,
} from "~/components/input/dropbox/common.js";
import { NAME as DROPBOX_OUTPUT_NAME } from "~/components/output/bytes/dropbox/element.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup, waitForOutputOption } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Dropbox | Diffuse" });

/**
 * @import { DropboxOutputElement } from "@specs/components/output/bytes/dropbox/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [inputConfigurator, outputOrchestrator, sourcesOrchestrator] =
  await Promise.all([
    foundation.configurator.input(),
    foundation.orchestrator.output(),
    foundation.orchestrator.sources(),
  ]);

await Promise.all([
  customElements.whenDefined(inputConfigurator.localName),
  customElements.whenDefined(outputOrchestrator.localName),
  customElements.whenDefined(sourcesOrchestrator.localName),
]);

const dropboxOption = await waitForOutputOption(outputOrchestrator, "Dropbox");
const OUTPUT_DROPBOX_ID = dropboxOption.id;

/** Look up the Dropbox output element, which may not exist yet. */
function dropboxOutputEl() {
  return /** @type {DropboxOutputElement | undefined} */ (
    outputOrchestrator.root().querySelector(DROPBOX_OUTPUT_NAME)
  );
}

////////////////////////////////////////////
// OAUTH TOKEN FROM HASH
////////////////////////////////////////////

const hashParams = new URLSearchParams(location.hash.slice(1));
let currentToken = hashParams.get("refresh_token");
const outputToken = hashParams.get("output_refresh_token");
const authError = hashParams.get("error");

if (currentToken || outputToken || authError) {
  history.replaceState({}, "", location.pathname + location.search);
}

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const { setItems, setError } = setup({
  title: "Dropbox",
  hasInput: false,
  hasOutput: false,

  description: html`
    <p>
      Add your Dropbox as an audio source, or use it as user-data storage.
      Authorize with Dropbox to get started.
    </p>
    <p class="caption">
      When used as user-data storage, a custom syncing strategy tracks what
      was added and removed so conflicts can be resolved.
    </p>
  `,

  rightContent: html`
    <div id="dropbox-auth-section" class="button-row">
      <button id="dropbox-auth-btn">
        <i class="ph-fill ph-music-notes"></i>
        Add audio input
      </button>
      <button id="dropbox-output-btn" class="button--brand">
        <i class="ph-fill ph-person"></i>
        Use as userdata storage
      </button>
    </div>

    <div id="dropbox-add-section" hidden>
      <label>
        Directory path
        <input id="dropbox-dir" placeholder="/">
      </label>
      <p class="caption">Leave empty to scan your entire Dropbox.</p>
      <div class="button-row">
        <button id="dropbox-add-btn">
          <i class="ph-fill ph-music-notes"></i>
          Add audio input
        </button>
      </div>
    </div>
  `,

  formFields: html`

  `,
  onSubmit: async () => {},
});

const authSection =
  /** @type {HTMLElement} */ (document.querySelector("#dropbox-auth-section"));
const outputBtn =
  /** @type {HTMLElement} */ (document.querySelector("#dropbox-output-btn"));
const addSection =
  /** @type {HTMLElement} */ (document.querySelector("#dropbox-add-section"));
const dirInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#dropbox-dir"));

if (currentToken) {
  authSection.hidden = true;
  addSection.hidden = false;
}

if (authError) {
  setError("Dropbox authorization failed. Please try again.");
}

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const inputSources = sourcesOrchestrator.sources()[SCHEME] ?? [];
  // Re-query the element each run so we pick it up when it's appended.
  const el = dropboxOutputEl();
  const outputToken = el?.refreshToken();
  const isSelectedOutput =
    outputOrchestrator.selected()?.id === OUTPUT_DROPBOX_ID;

  // Hide the "Use as userdata storage" button when the output is
  // already selected. When authorized but not selected, the button
  // stays visible so the user can re-select.
  if (outputBtn) outputBtn.hidden = isSelectedOutput;

  /** @type {{ name: string; detail: string; isInput: boolean; isOutput: boolean; isSelectedOutput: boolean; isDisabled?: boolean; onRemove: () => void; onToggleDisabled?: () => void }[]} */
  const items = [];

  for (const source of inputSources) {
    const parsed = parseURI(source.uri);
    if (!parsed) continue;

    items.push({
      name: source.label,
      detail: "Dropbox",
      isInput: true,
      isOutput: false,
      isSelectedOutput: false,
      isDisabled: sourcesOrchestrator.isDisabled(source.uri),
      onRemove: () => removeSource(source.uri),
      onToggleDisabled: () => sourcesOrchestrator.toggle(source.uri),
    });
  }

  if (outputToken) {
    items.push({
      name: "Dropbox",
      detail: "App folder",
      isInput: false,
      isOutput: true,
      isSelectedOutput,
      onRemove: () => removeOutput(),
    });
  }

  setItems(items);
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

document.querySelector("#dropbox-auth-btn")?.addEventListener("click", () => {
  const dropboxInput = /** @type {import("~/components/input/dropbox/element.js").default} */ (inputConfigurator.inputs()[SCHEME]);
  dropboxInput.authorize();
});

document.querySelector("#dropbox-output-btn")?.addEventListener(
  "click",
  async () => {
    const el = dropboxOutputEl();
    const existingToken = el?.refreshToken();
    if (existingToken) {
      // Already authorized — just select the output.
      await outputOrchestrator.select(OUTPUT_DROPBOX_ID);
    } else {
      // Not yet authorized — start the OAuth flow.
      el?.authorize();
    }
  },
);

document.querySelector("#dropbox-add-btn")?.addEventListener(
  "click",
  async () => {
    if (!currentToken) return;

    setError(null);
    try {
      const rawDir = dirInput?.value?.trim() || "/";
      const directoryPath = rawDir.startsWith("/") ? rawDir : "/" + rawDir;

      const account = { refreshToken: currentToken, directoryPath };
      const uri = buildURI(account);
      const now = new Date().toISOString();

      const tracksCol = outputOrchestrator.tracks.collection();
      const existingTracks = tracksCol.state === "loaded" ? tracksCol.data : [];

      await outputOrchestrator.tracks.save([
        ...existingTracks,
        {
          $type: "sh.diffuse.output.track",
          id: TID.now(),
          createdAt: now,
          updatedAt: now,
          kind: "placeholder",
          uri,
        },
      ]);

      // Reset UI after adding
      if (dirInput) dirInput.value = "";
      currentToken = null;
      authSection.hidden = false;
      addSection.hidden = true;
    } catch (err) {
      setError(
        err instanceof Error ? err.message : "Failed to add Dropbox source",
      );
    }
  },
);

/** @param {string} uri */
async function removeSource(uri) {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove source");
  }
}

async function removeOutput() {
  setError(null);
  try {
    await outputOrchestrator.deselect();
    await dropboxOutputEl()?.unsetRefreshToken();
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to disconnect Dropbox");
  }
}

////////////////////////////////////////////
// HANDLE OUTPUT OAUTH RETURN
////////////////////////////////////////////

// When returning from the Dropbox output OAuth flow, store the token and
// select the output.
if (outputToken) {
  setError(null);
  try {
    await dropboxOutputEl()?.setRefreshToken(outputToken);
    await outputOrchestrator.select(OUTPUT_DROPBOX_ID);
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to connect Dropbox");
  }
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
