import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/dropbox/constants.js";
import {
  accountId,
  buildURI,
  parseURI,
} from "~/components/input/dropbox/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Dropbox | Diffuse" });

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

////////////////////////////////////////////
// OAUTH TOKEN FROM HASH
////////////////////////////////////////////

const hashParams = new URLSearchParams(location.hash.slice(1));
let currentToken = hashParams.get("refresh_token");

if (currentToken) {
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
      Add your Dropbox as an audio source. Authorize with Dropbox, then optionally
      specify which directory to scan for music.
    </p>
  `,

  rightContent: html`
    <div id="dropbox-auth-section" class="button-row">
      <button id="dropbox-auth-btn">
        <i class="ph-fill ph-cloud-arrow-up"></i>
        Authorize with Dropbox
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
const addSection =
  /** @type {HTMLElement} */ (document.querySelector("#dropbox-add-section"));
const dirInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#dropbox-dir"));

if (currentToken) {
  authSection.hidden = true;
  addSection.hidden = false;
}

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const inputSources = sourcesOrchestrator.sources()[SCHEME] ?? [];

  /** @type {Map<string, { label: string; uri: string }>} */
  const allSources = new Map();

  for (const source of inputSources) {
    const parsed = parseURI(source.uri);
    if (!parsed) continue;

    const id = accountId(parsed);
    if (!allSources.has(id)) {
      allSources.set(id, { label: source.label, uri: source.uri });
    }
  }

  setItems(
    [...allSources.values()].map(({ label, uri }) => ({
      name: label,
      detail: "Dropbox",
      isInput: true,
      isOutput: false,
      isSelectedOutput: false,
      isDisabled: sourcesOrchestrator.isDisabled(uri),
      onRemove: () => removeSource(uri),
      onToggleDisabled: () => sourcesOrchestrator.toggle(uri),
    })),
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

document.querySelector("#dropbox-auth-btn")?.addEventListener("click", () => {
  const dropboxInput = /** @type {import("~/components/input/dropbox/element.js").default} */ (inputConfigurator.inputs()[SCHEME]);
  dropboxInput.authorize();
});

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

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
