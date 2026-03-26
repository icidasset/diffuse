import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/local/constants.js";
import { isSupported } from "~/components/input/local/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 */

foundation.setup({ title: "Connect Local | Diffuse" });

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

const localInput =
  /** @type {import("~/components/input/local/element.js").CLASS} */ (inputConfigurator
    .inputs?.()[SCHEME]);

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const supported = isSupported();

const { setItems, setError } = setup({
  title: "Local files",
  hasInput: false,
  hasOutput: false,

  description: html`
    <p>Add local directories or files as audio input.</p>
    ${supported
      ? html`
        <div class="button-row">
          <wa-button id="local-add-dir-btn" variant="neutral" appearance="filled">
            <wa-icon slot="start" library="phosphor/fill" name="folder-open"></wa-icon>
            Add directory
          </wa-button>
          <wa-button id="local-add-files-btn" variant="neutral" appearance="filled">
            <wa-icon slot="start" library="phosphor/fill" name="music-notes"></wa-icon>
            Add files
          </wa-button>
        </div>
      `
      : html`
        <wa-callout variant="warning">
          Your browser does not support the File System Access API. Use a Chromium-based
          browser to add local files.
        </wa-callout>
      `}
  `,

  formFields: html`

  `,
  onSubmit: async () => {},
});

document
  .querySelector("#local-add-dir-btn")
  ?.addEventListener("click", () => addDirectory());

document
  .querySelector("#local-add-files-btn")
  ?.addEventListener("click", () => addFiles());

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];
  const entries = localInput?.sources(tracks) ?? [];

  setItems(
    entries.map(({ label, uri }) => ({
      name: label,
      detail: "local",
      isInput: true,
      isOutput: false,
      isSelectedOutput: false,
      onRemove: () => removeEntry(uri),
    })),
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {string} uri */
async function removeEntry(uri) {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove entry");
  }
}

async function addDirectory() {
  setError(null);
  try {
    const uri = await localInput.addDirectory();
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
  } catch (err) {
    if (err instanceof Error && err.name !== "AbortError") {
      setError(err.message);
    }
  }
}

async function addFiles() {
  setError(null);
  try {
    const uris = await localInput.addFiles();
    const now = new Date().toISOString();
    const tracksCol = outputOrchestrator.tracks.collection();
    const existingTracks = tracksCol.state === "loaded" ? tracksCol.data : [];
    await outputOrchestrator.tracks.save([
      ...existingTracks,
      ...uris.map((uri) => {
        /** @type {Track} */
        const track = {
          $type: "sh.diffuse.output.track",
          id: TID.now(),
          createdAt: now,
          updatedAt: now,
          kind: "placeholder",
          uri,
        };

        return track;
      }),
    ]);
  } catch (err) {
    if (err instanceof Error && err.name !== "AbortError") {
      setError(err.message);
    }
  }
}
