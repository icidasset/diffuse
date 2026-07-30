import { html, render as litRender } from "lit-html";

import * as Output from "~/common/output.js";
import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

import { SCHEME as SCHEME_DROPBOX } from "~/components/input/dropbox/constants.js";
import { SCHEME as SCHEME_EPHEMERAL_CACHE } from "~/components/input/ephemeral-cache/constants.js";
import { SCHEME as SCHEME_HTTPS } from "~/components/input/https/constants.js";
import { SCHEME as SCHEME_ICECAST } from "~/components/input/icecast/constants.js";
import { SCHEME as SCHEME_LOCAL } from "~/components/input/local/constants.js";
import { SCHEME as SCHEME_OPENSUBSONIC } from "~/components/input/opensubsonic/constants.js";
import { SCHEME as SCHEME_S3 } from "~/components/input/s3/constants.js";

/** @type {Record<string, string>} */
const SCHEME_NAMES = {
  [SCHEME_DROPBOX]: "Dropbox",
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

const [
  inputConfigurator,
  sourcesOrchestrator,
  outputOrchestrator,
  processOrchestrator,
] = await Promise.all([
  foundation.configurator.input(),
  foundation.orchestrator.sources(),
  foundation.orchestrator.output(),
  foundation.orchestrator.processTracks({ disableWhenReady: true }),
]);

await Promise.all([
  customElements.whenDefined(inputConfigurator.localName),
  customElements.whenDefined(sourcesOrchestrator.localName),
  customElements.whenDefined(outputOrchestrator.localName),
]);

////////////////////////////////////////////
// PROCESS BUTTON
////////////////////////////////////////////

const processBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#process-btn"));
const processIcon =
  /** @type {HTMLElement} */ (document.querySelector("#process-icon"));
const processLabel =
  /** @type {HTMLElement} */ (document.querySelector("#process-label"));

effect(() => {
  const isProcessing = processOrchestrator.isProcessing();
  const { processed, total } = processOrchestrator.progress();
  const pct = total > 0 ? Math.round((processed / total) * 100) : null;

  processBtn.disabled = isProcessing;
  processIcon.className = isProcessing
    ? "ph-fill ph-arrows-clockwise animate-spin"
    : "ph-fill ph-arrows-clockwise";
  processLabel.textContent = isProcessing
    ? (pct !== null ? `Processing (${pct}%)` : "Listing")
    : "Process";
});

processBtn.addEventListener("click", async () => {
  const output = await foundation.orchestrator.output();
  await Output.data(output.tracks);
  await processOrchestrator.process();
});

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const list =
  /** @type {HTMLElement} */ (document.querySelector("#sources-list"));
const empty =
  /** @type {HTMLElement} */ (document.querySelector("#sources-empty"));

/** @param {string} uri */
const trackPrefix = (uri) => {
  const q = uri.indexOf("?");
  return q === -1 ? uri : uri.slice(0, q);
};

////////////////////////////////////////////
// ONLINE STATUS
////////////////////////////////////////////


const onlineMap = signal(/** @type {Record<string, boolean | null>} */ ({}));

/** @param {{ [scheme: string]: import("@specs/components/input/types.d.ts").Source[] }} sourcesRecord */
async function checkOnlineStatus(sourcesRecord) {
  const sources = Object.values(sourcesRecord).flat();
  const entries = await Promise.all(
    sources.map(async ({ uri }) => {
      const result = await inputConfigurator.consult(uri);
      // `cachedConsult` normalises `"unsure"` (transient consult failure)
      // to `"no"` without caching it — so a brief network blip after laptop
      // wake shows "Offline" transiently and flips back to "Online" on the
      // next consult, rather than sticking "Offline" for the full TTL.
      const online =
        result.supported && result.consult !== "undetermined"
          ? result.consult === "yes"
          : null;
      return /** @type {[string, boolean | null]} */ ([trackPrefix(uri), online]);
    }),
  );
  onlineMap.value = Object.fromEntries(entries);
}

effect(() => {
  checkOnlineStatus(sourcesOrchestrator.sources());
});

effect(() => {
  const sourcesRecord = sourcesOrchestrator.sources();
  const statusMap = onlineMap.get();

  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

  /** @param {string} uri */
  const statusClass = (uri) => {
    const status = statusMap[trackPrefix(uri)];
    if (status === true) return "sources-item__status--online";
    if (status === false) return "sources-item__status--offline";
    return "sources-item__status--unknown";
  };

  /** @param {string} uri */
  const statusTitle = (uri) => {
    const status = statusMap[trackPrefix(uri)];
    if (status === true) return "Online";
    if (status === false) return "Offline";
    return "Status unknown";
  };

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
          const isDisabled = sourcesOrchestrator.isDisabled(uri);
          const trackCount = tracks.filter((t) => t.uri.startsWith(uri)).length;
          return html`
            <li class="sources-scheme">${SCHEME_NAMES[scheme] ?? scheme}</li>
            <li class="sources-item ${isDisabled
              ? "sources-item--disabled"
              : ""}">
              <div class="sources-item__info">
                <span class="sources-item__name">Files stored in the browser</span>
                <span class="sources-item__detail">
                  <span class="sources-item__status ${statusClass(uri)}" title="${statusTitle(uri)}"></span>
                  ${trackCount} track${trackCount === 1 ? "" : "s"}
                </span>
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
            const isDisabled = sourcesOrchestrator.isDisabled(uri);
            const trackCount = tracks.filter((t) =>
              t.uri.startsWith(trackPrefix(uri))
            ).length;
            return html`
              <li class="sources-item ${isDisabled
                ? "sources-item--disabled"
                : ""}">
                <div class="sources-item__info">
                  <span class="sources-item__name">${label}</span>
                  <span class="sources-item__detail">
                    <span class="sources-item__status ${statusClass(uri)}" title="${statusTitle(uri)}"></span>
                    ${trackCount} track${trackCount === 1 ? "" : "s"}
                  </span>
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
                  @click="${() => removeSource(uri, label)}"
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
  return removeSource(SCHEME_EPHEMERAL_CACHE, "Files stored in the browser");
}

/**
 * @param {string} uri
 * @param {string} name
 */
async function removeSource(uri, name) {
  if (!confirm(`Are you sure you want to remove the source "${name}"?`)) return;

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
