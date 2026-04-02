import { html, render as litRender } from "lit-html";

import * as Output from "~/common/output.js";
import foundation from "~/common/foundation.js";
import { computed, effect } from "~/common/signal.js";

import { CLASS as Sources } from "~/components/orchestrator/sources/element.js";

import { SCHEME as SCHEME_EPHEMERAL_CACHE } from "~/components/input/ephemeral-cache/constants.js";
import { SCHEME as SCHEME_HTTPS } from "~/components/input/https/constants.js";
import { SCHEME as SCHEME_ICECAST } from "~/components/input/icecast/constants.js";
import { SCHEME as SCHEME_LOCAL } from "~/components/input/local/constants.js";
import { SCHEME as SCHEME_OPENSUBSONIC } from "~/components/input/opensubsonic/constants.js";
import { SCHEME as SCHEME_S3 } from "~/components/input/s3/constants.js";

/** @type {Record<string, string>} */
const SCHEME_NAMES = {
  [SCHEME_EPHEMERAL_CACHE]: "Browser storage",
  [SCHEME_HTTPS]: "HTTPS",
  [SCHEME_ICECAST]: "Icecast",
  [SCHEME_LOCAL]: "Local directories & files",
  [SCHEME_OPENSUBSONIC]: "OpenSubsonic",
  [SCHEME_S3]: "S3",
};

foundation.setup({ title: "Sources | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [inputConfigurator, sourcesOrchestrator, outputOrchestrator] =
  await Promise.all([
    foundation.configurator.input(),
    foundation.orchestrator.sources(),
    foundation.orchestrator.output(),
  ]);

await Promise.all([
  customElements.whenDefined(inputConfigurator.localName),
  customElements.whenDefined(sourcesOrchestrator.localName),
  customElements.whenDefined(outputOrchestrator.localName),
]);

const disabledSources = computed(() => {
  const col = outputOrchestrator.settings.collection();
  if (col.state !== "loaded") return /** @type {string[]} */ ([]);

  const setting = col.data.find((s) => s.key === Sources.DISABLED_KEY);
  if (!setting) return /** @type {string[]} */ ([]);

  try {
    const parsed = JSON.parse(setting.value);
    return Array.isArray(parsed) ? /** @type {string[]} */ (parsed) : [];
  } catch {
    return /** @type {string[]} */ ([]);
  }
});

/**
 * Returns the part of a source URI suitable for `startsWith` matching against
 * track URIs. Some inputs (e.g. OpenSubsonic) include query params in the
 * source URI that won't appear at the start of a track URI, so we strip them.
 *
 * @param {string} uri
 */
function trackPrefix(uri) {
  const q = uri.indexOf("?");
  return q === -1 ? uri : uri.slice(0, q);
}

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const list =
  /** @type {HTMLElement} */ (document.querySelector("#sources-list"));
const empty =
  /** @type {HTMLElement} */ (document.querySelector("#sources-empty"));

effect(() => {
  const sourcesRecord = sourcesOrchestrator.sources();
  const disabled = disabledSources();

  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

  const entries = Object.entries(sourcesRecord).filter(
    ([, sources]) => sources.length > 0,
  );

  list.hidden = entries.length === 0;
  empty.hidden = entries.length > 0;

  litRender(
    html`
      ${entries.map(([scheme, sources]) => {
        if (scheme === SCHEME_EPHEMERAL_CACHE) {
          const uri = `${SCHEME_EPHEMERAL_CACHE}://`;
          const isDisabled = disabled.includes(uri);
          const trackCount = tracks.filter((t) =>
            t.uri.startsWith(trackPrefix(uri))
          ).length;
          return html`
            <li class="sources-scheme">${SCHEME_NAMES[scheme] ?? scheme}</li>
            <li class="sources-item ${isDisabled
              ? "sources-item--disabled"
              : ""}">
              <div class="sources-item__info">
                <span class="sources-item__name">Files stored in the browser</span>
                <span class="sources-item__detail">${trackCount} track${trackCount ===
                    1
                  ? ""
                  : "s"}</span>
              </div>
              <button
                class="button--plain"
                title="${isDisabled ? "Enable source" : "Disable source"}"
                @click="${() => sourcesOrchestrator.toggle(uri)}"
              >
                <i class="ph-fill ${isDisabled
                  ? "ph-eye-slash"
                  : "ph-eye"}"></i>
              </button>
              <button
                class="button--plain button--icon"
                title="Remove source"
                @click="${() => removeEphemeralSources()}"
              >
                <i class="ph-fill ph-skull"></i>
              </button>
            </li>
          `;
        }

        return html`
          <li class="sources-scheme">${SCHEME_NAMES[scheme] ?? scheme}</li>
          ${sources.map(({ label, uri }) => {
            const isDisabled = disabled.includes(trackPrefix(uri));
            const trackCount = tracks.filter((t) =>
              t.uri.startsWith(trackPrefix(uri))
            ).length;
            return html`
              <li class="sources-item ${isDisabled
                ? "sources-item--disabled"
                : ""}">
                <div class="sources-item__info">
                  <span class="sources-item__name">${label}</span>
                  <span class="sources-item__detail">${trackCount} track${trackCount ===
                      1
                    ? ""
                    : "s"}</span>
                </div>
                <button
                  class="button--plain button--icon"
                  title="${isDisabled ? "Enable source" : "Disable source"}"
                  @click="${() => sourcesOrchestrator.toggle(uri)}"
                >
                  <i class="ph-fill ${isDisabled
                    ? "ph-eye-slash"
                    : "ph-eye"}"></i>
                </button>
                <button
                  class="button--plain button--icon"
                  title="Remove source"
                  @click="${() => removeSource(uri)}"
                >
                  <i class="ph-fill ph-skull"></i>
                </button>
              </li>
            `;
          })}
        `;
      })}
    `,
    list,
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

async function removeEphemeralSources() {
  return removeSource(SCHEME_EPHEMERAL_CACHE);
}

/** @param {string} uri */
async function removeSource(uri) {
  const tracks = await Output.data(outputOrchestrator.tracks);

  const detachedTracks = await inputConfigurator.detach({
    fileUriOrScheme: uri,
    tracks,
  });

  if (detachedTracks) await outputOrchestrator.tracks.save(detachedTracks);
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
