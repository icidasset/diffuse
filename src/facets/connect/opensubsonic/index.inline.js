import "@awesome.me/webawesome/dist/components/input/input.js";
import "@awesome.me/webawesome/dist/components/select/select.js";
import "@awesome.me/webawesome/dist/components/option/option.js";

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
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 * @import { default as WaSelect } from "@awesome.me/webawesome/dist/components/select/select.js"
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
    <p class="wa-caption-xs">
      Supports authentication via username + password or an API key.
    </p>
  `,

  formFields: html`
    <wa-input
      id="oss-host"
      label="Host"
      placeholder="music.example.com"
      required
    ></wa-input>
    <wa-select id="oss-tls" label="Use HTTPS / TLS?" value="true">
      <wa-option value="true">Yes</wa-option>
      <wa-option value="false">No</wa-option>
    </wa-select>
    <wa-input id="oss-username" label="Username"></wa-input>
    <wa-input id="oss-password" label="Password" type="password"></wa-input>
    <p class="wa-caption-xs">Or use an API key instead of username + password:</p>
    <wa-input id="oss-apikey" label="API key" type="password"></wa-input>
    <p class="wa-caption-xs">* Host is required</p>
  `,

  onSubmit: () => addServer(),
});

const hostInput = /** @type {WaInput} */ (document.querySelector("#oss-host"));
const tlsSelect = /** @type {WaSelect} */ (document.querySelector("#oss-tls"));
const usernameInput =
  /** @type {WaInput} */ (document.querySelector("#oss-username"));
const passwordInput =
  /** @type {WaInput} */ (document.querySelector("#oss-password"));
const apikeyInput =
  /** @type {WaInput} */ (document.querySelector("#oss-apikey"));

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
