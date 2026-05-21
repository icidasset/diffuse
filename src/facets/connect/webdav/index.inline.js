import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/webdav/constants.js";
import { buildURI, parseURI, serverId } from "~/components/input/webdav/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

/**
 * @import { Server } from "@specs/components/input/webdav/types.d.ts"
 */

foundation.setup({ title: "Connect WebDAV | Diffuse" });

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
  title: "WebDAV",
  hasOutput: false,

  description: html`
    <p>
      Connect to a WebDAV server to use it as an audio source.
    </p>
  `,

  formFields: html`
    <label>Host* <input id="webdav-host" placeholder="music.example.com" required></label>
    <label>Directory <input id="webdav-dir" placeholder="/"></label>
    <label>Username <input id="webdav-username"></label>
    <label>Password <input id="webdav-password" type="password"></label>
    <p class="caption">* Required</p>
  `,

  onSubmit: () => addServer(),
});

const hostInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#webdav-host"));
const dirInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#webdav-dir"));
const usernameInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#webdav-username"));
const passwordInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#webdav-password"));

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
  const username = usernameInput.value?.trim() || "";
  const password = passwordInput.value?.trim() || "";

  if (!host) return;

  /** @type {Server} */
  const server = { host, dir, username, password };
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
