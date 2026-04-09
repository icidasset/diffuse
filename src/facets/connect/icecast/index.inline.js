import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/icecast/constants.js";
import { buildURI, parseURI } from "~/components/input/icecast/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

foundation.setup({ title: "Connect Icecast | Diffuse" });

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
  title: "Icecast",
  hasOutput: false,

  description: html`
    <p>Add an Icecast stream as audio input.</p>
  `,

  formFields: html`
    <label>Stream URL <input id="icecast-url" type="url" placeholder="https://example.com/stream" required></label>
  `,

  onSubmit: () => addStream(),
});

const urlInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#icecast-url"));

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const inputSources = sourcesOrchestrator.sources()[SCHEME] ?? [];

  setItems(
    inputSources.map((source) => {
      const parsed = parseURI(source.uri);
      return {
        name: parsed?.streamUrl ?? source.uri,
        detail: parsed?.host ?? "",
        isInput: true,
        isOutput: false,
        isSelectedOutput: false,
        isDisabled: sourcesOrchestrator.isDisabled(source.uri),
        onRemove: () => removeStream(source.uri),
        onToggleDisabled: () => sourcesOrchestrator.toggle(source.uri),
      };
    }),
  );
});
////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {string} uri */
async function removeStream(uri) {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove stream");
  }
}

async function addStream() {
  const rawUrl = urlInput.value?.trim();
  if (!rawUrl) return;

  const uri = buildURI(rawUrl);
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
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
