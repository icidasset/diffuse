import "@awesome.me/webawesome/dist/components/input/input.js";

import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/https/constants.js";
import { parseURI } from "~/components/input/https/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

/**
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 */

document.title = "Connect HTTPS | Diffuse";

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
// UI
////////////////////////////////////////////

const { setItems, setError } = setup({
  title: "HTTPS",
  hasOutput: false,

  description: html`
    <p>Add HTTPS urls as audio sources.</p>
  `,

  formFields: html`
    <wa-input
      id="https-url"
      label="URL"
      type="url"
      placeholder="https://example.com/audio.mp3"
      required
    ></wa-input>
  `,

  onSubmit: () => addUrl(),
});

const urlInput = /** @type {WaInput} */ (document.querySelector("#https-url"));

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const inputSources = sourcesOrchestrator.sources()[SCHEME] ?? [];

  setItems(
    inputSources.map((source) => {
      const parsed = parseURI(source.uri);
      return {
        name: source.uri,
        detail: parsed?.host ?? "",
        isInput: true,
        isOutput: false,
        isSelectedOutput: false,
        onRemove: () => removeUrl(source.uri),
      };
    }),
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {string} uri */
async function removeUrl(uri) {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove URL");
  }
}

async function addUrl() {
  const url = urlInput.value?.trim();
  if (!url) return;

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
      uri: url,
    },
  ]);
}
