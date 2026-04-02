import * as TID from "@atcute/tid";
import { html } from "lit-html";

import * as Output from "~/common/output.js";
import { SCHEME } from "~/components/input/opensubsonic/constants.js";
import {
  buildURI,
  parseURI,
  serverId,
} from "~/components/input/opensubsonic/common.js";
import { effect } from "~/common/signal.js";
import foundation from "~/common/foundation.js";

import { setup } from "~/facets/connect/common.js";

/**
 * @import { Server } from "~/components/input/opensubsonic/types.d.ts"
 */

foundation.setup({ title: "Connect OpenSubsonic | Diffuse" });

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
  title: "OpenSubsonic",
  hasOutput: false,

  description: html`
    <p>
      Connect to an OpenSubsonic server to use it as audio input.
    </p>
    <p class="caption">
      Supports authentication via username + password, or an API key.
    </p>
  `,

  formFields: html`
    <label>Host <input id="oss-host" placeholder="music.example.com" required></label>
    <label>Use HTTPS / TLS?
      <select id="oss-tls">
        <option value="true" selected>Yes</option>
        <option value="false">No</option>
      </select>
    </label>
    <label>Username <input id="oss-username"></label>
    <label>Password <input id="oss-password" type="password"></label>
    <p class="caption">Or use an API key instead of username + password:</p>
    <label>API key <input id="oss-apikey" type="password"></label>
    <p class="caption">* Host is required</p>
  `,

  onSubmit: () => addServer(),
});

const hostInput = /** @type {HTMLInputElement} */ (document.querySelector("#oss-host"));
const tlsSelect = /** @type {HTMLSelectElement} */ (document.querySelector("#oss-tls"));
const usernameInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#oss-username"));
const passwordInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#oss-password"));
const apikeyInput =
  /** @type {HTMLInputElement} */ (document.querySelector("#oss-apikey"));

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
      detail: server.tls ? "https" : "http",
      isInput: true,
      isOutput: false,
      isSelectedOutput: false,
      onRemove: () => removeServer(uri),
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
  const tls = tlsSelect.value !== "false";
  const username = usernameInput.value?.trim() || undefined;
  const password = passwordInput.value?.trim() || undefined;
  const apiKey = apikeyInput.value?.trim() || undefined;

  if (!host) return;

  /** @type {Server} */
  const server = { host, tls, username, password, apiKey };
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
