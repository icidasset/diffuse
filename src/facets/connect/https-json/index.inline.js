import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/https-json/constants.js";
import { buildURI, parseURI, serverId } from "~/components/input/https-json/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

/**
 * @import { Server } from "~/components/input/https-json/common.js"
 */

foundation.setup({ title: "Connect HTTPS / JSON Listing | Diffuse" });

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
  title: "HTTPS + JSON",
  hasOutput: false,

  description: html`
    <p>
      Connect to an HTTPS server that serves JSON directory listings
      (eg. static-web-server with <code>--directory-listing-format=json</code>).
    </p>

    <p><small>Uses the format: <code>[{ "name": "Example folder", "type": "directory" }, { "name": "file.mp3", "type": "file" }]</code></small></p>
  `,

  formFields: html`
    <label>Host* <input id="https-json-host" placeholder="music.example.com" required></label>
    <label>Directory <input id="https-json-dir" placeholder="/"></label>
    <label>Excluded directories <input id="https-json-exclude" placeholder="private, tmp"></label>
    <p class="caption">* Required.<br />Excluded directories are matched by name at any depth, comma-separated.</p>
  `,

  onSubmit: () => addServer(),
});

const hostInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#https-json-host"));
const dirInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#https-json-dir"));
const excludeInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#https-json-exclude"));

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

effect(() => {
  const inputSources = sourcesOrchestrator.sources()[SCHEME] ?? [];

  /** @type {Map<string, { server: Server; uri: string }>} */
  const allServers = new Map();

  for (const source of inputSources) {
    const parsed = parseURI(source.uri);
    if (!parsed) continue;

    const id = serverId(parsed.server);
    if (!allServers.has(id)) {
      allServers.set(id, { server: parsed.server, uri: source.uri });
    }
  }

  setItems(
    [...allServers.values()].map(({ server, uri }) => ({
      name: server.host,
      detail: server.dir,
      isInput: true,
      isOutput: false,
      isSelectedOutput: false,
      isDisabled: sourcesOrchestrator.isDisabled(uri),
      onRemove: () => removeServer(uri),
      onToggleDisabled: () => sourcesOrchestrator.toggle(uri),
    })),
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {string} uri */
async function removeServer(uri) {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove server");
  }
}

async function addServer() {
  const host = hostInput.value?.trim();
  const dir = dirInput.value?.trim() || "/";
  const exclude = (excludeInput.value ?? "").split(",").map((s) => s.trim()).filter(Boolean);

  if (!host) return;

  /** @type {Server} */
  const server = { host, dir, exclude: exclude.length ? exclude : undefined };
  const uri = buildURI(server);

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
