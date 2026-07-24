import * as TID from "@atcute/tid";
import * as IDB from "idb-keyval";
import { html, render as litRender } from "lit-html";

import * as Output from "~/common/output.js";
import {
  CACHE_KEY_PREFIX,
  SCHEME as SCHEME_EPHEMERAL_CACHE,
} from "~/components/input/ephemeral-cache/constants.js";
import { SCHEME as SCHEME_DROPBOX } from "~/components/input/dropbox/constants.js";
import { effect, signal } from "~/common/signal.js";
import { safeDecodeURIComponent } from "~/common/utils.js";
import foundation from "~/common/foundation.js";

/**
 * @import { TemplateResult } from "lit-html"
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {UploadElement} from "@specs/components/upload/types.d.ts"
 */

/** Human-readable labels for upload component schemes. */
/** @type {Record<string, string>} */
const SCHEME_LABELS = {
  [SCHEME_DROPBOX]: "Dropbox",
};

/** Directory in the cloud where uploaded files are stored. */
const UPLOAD_DIRECTORY = "/Diffuse";

/** IDB key for persisting the sync scheme across sessions. */
const SYNC_SCHEME_KEY = "file-manager:sync-scheme";

/** IDB key for storing a pending delete path (retried after reconnection). */
const PENDING_DELETE_KEY = "file-manager:pending-delete";

foundation.setup({ title: "File Manager | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const [
  inputConfigurator,
  outputOrchestrator,
  uploadConfigurator,
  sourcesOrchestrator,
  processOrchestrator,
] = await Promise.all([
  foundation.configurator.input(),
  foundation.orchestrator.output(),
  foundation.configurator.upload(),
  foundation.orchestrator.sources(),
  foundation.orchestrator.processTracks({ disableWhenReady: true }),
]);

await Promise.all([
  customElements.whenDefined(inputConfigurator.localName),
  customElements.whenDefined(outputOrchestrator.localName),
  customElements.whenDefined(uploadConfigurator.localName),
  customElements.whenDefined(sourcesOrchestrator.localName),
  customElements.whenDefined(processOrchestrator.localName),
]);

/** Maps ephemeral cache URIs to their original filename. */
const ephemeralNames = new Map();

/** Bumped whenever `ephemeralNames` is updated so the reactive effect re-fires. */
const namesVersion = signal(0);

/** Tracks the upload button state: "idle", "caching", or "uploading". */
const uploadState = signal(/** @type {"idle" | "caching" | "uploading"} */ ("idle"));

/** The scheme currently being synced with (e.g. "dropbox"), or null. */
const syncScheme = signal(/** @type {string | null} */ (null));

////////////////////////////////////////////
// UI
////////////////////////////////////////////

/**
 * @typedef {{ name: string; onRemove: () => void }} FileItem
 */

/**
 * Renders the File Manager layout (left: logo, title, description; right: the
 * supplied content, an error callout, and reactive lists of items) and returns
 * helpers to update it.
 *
 * @param {Object} config
 * @param {string} config.title
 * @param {TemplateResult | string} config.description
 * @param {TemplateResult} [config.leftContent]
 * @param {TemplateResult} config.rightContent
 * @returns {{ setLocalItems: (items: FileItem[]) => void, setRemoteItems: (items: FileItem[]) => void, setError: (message: string | null) => void }}
 */
function setup({ title, description, leftContent, rightContent }) {
  const main = document.querySelector("main");
  if (!main) throw new Error("No <main> element");

  litRender(
    html`
      <div class="facet__left">
        <div>
          <a href="./dashboard/" class="diffuse-logo-container">
            <svg viewBox="0 0 902 134" width="160">
              <title>Diffuse</title>
              <use
                xlink:href="images/diffuse-current.svg#diffuse"
                href="images/diffuse-current.svg#diffuse"
              ></use>
            </svg>
          </a>
        </div>
        <h1>${title}</h1>
        ${description}
        ${leftContent}
      </div>
      <div class="facet__right">
        ${rightContent}
        <div id="file-card-error" class="callout callout--danger" hidden></div>
        <div id="local-section" class="file-section" hidden>
          <h2 class="file-section__title">Local tracks</h2>
          <ul id="local-list" class="file-list"></ul>
        </div>
        <div id="remote-section" class="file-section" hidden>
          <h2 class="file-section__title">Remote tracks</h2>
          <ul id="remote-list" class="file-list"></ul>
        </div>
      </div>
    `,
    main,
  );

  const cardErrorEl =
    /** @type {HTMLElement} */ (main.querySelector("#file-card-error"));
  const localSection =
    /** @type {HTMLElement} */ (main.querySelector("#local-section"));
  const remoteSection =
    /** @type {HTMLElement} */ (main.querySelector("#remote-section"));
  const localList =
    /** @type {HTMLElement} */ (main.querySelector("#local-list"));
  const remoteList =
    /** @type {HTMLElement} */ (main.querySelector("#remote-list"));

  /** @param {string | null} message */
  const setError = (message) => {
    cardErrorEl.hidden = message === null;
    cardErrorEl.textContent = message;
  };

  /** @param {FileItem[]} items */
  const setLocalItems = (items) => {
    localSection.hidden = items.length === 0;
    renderFileList(localList, items, "local");
  };

  /** @param {FileItem[]} items */
  const setRemoteItems = (items) => {
    remoteSection.hidden = items.length === 0;
    renderFileList(remoteList, items, "remote");
  };

  return { setLocalItems, setRemoteItems, setError };
}

/**
 * Renders a list of file items into the given list element.
 *
 * @param {HTMLElement} listEl
 * @param {FileItem[]} items
 * @param {string} prefix - Unique prefix for popover IDs to avoid collisions.
 */
function renderFileList(listEl, items, prefix) {
  litRender(
    html`
      ${items.map(
        ({ name, onRemove }, index) =>
          html`
            <li class="file-item">
              <div class="file-item__info">
                <span class="file-item__name">${name}</span>
              </div>
              <button
                class="button--plain button--icon"
                aria-label="More"
                popovertarget="${prefix}-menu-${index}"
              >
                <i class="ph-fill ph-dots-three-outline-vertical"></i>
              </button>
              <div id="${prefix}-menu-${index}" class="dropdown" popover>
                <button
                  @click="${(/** @type {MouseEvent} */ e) => {
                    /** @type {HTMLElement | null} */ (/** @type {HTMLElement} */ (e.currentTarget).closest("[popover]"))?.hidePopover();
                    onRemove();
                  }}"
                >
                  <i class="ph-fill ph-skull"></i>
                  Delete
                </button>
              </div>
            </li>
          `,
      )}
    `,
    listEl,
  );
}

const { setLocalItems, setRemoteItems, setError } = setup({
  title: "File Manager",
  description: html`
    <p>Upload audio files to play them and optionally sync them with your cloud storage. These are automatically added as inputs (aka. sources).</p>
  `,

  leftContent: html`
    <div class="button-row" style="align-items: center; margin-top: var(--space-md)">
      <button id="upload-btn" popovertarget="upload-menu">
        <i id="upload-icon" class="ph-fill ph-cloud-arrow-up"></i>
        <span id="upload-label">Upload tracks</span>
      </button>
      <div id="upload-menu" class="dropdown" popover></div>
      <button id="stop-sync-btn" class="button--danger" hidden>
      <i class="ph-fill ph-cloud-slash"></i>
        <span id="stop-sync-label">Stop syncing</span>
      </button>
      <button id="process-btn">
        <i id="process-icon" class="ph-fill ph-arrows-clockwise"></i>
        <span id="process-label">Process</span>
      </button>
      <i id="upload-indicator" class="ph-bold ph-spinner animate-spin upload-indicator" hidden></i>
    </div>
  `,

  rightContent: html`
    <label class="dropzone" id="local-dropzone">
      <input id="local-dropzone-input" type="file" accept="audio/*" multiple hidden />
      <i class="ph-bold ph-upload-simple"></i>
      <span>Drop or click to select files</span>
    </label>
  `,
});

const dropzone = document.querySelector("#local-dropzone");
const dropzoneInput =
  /** @type {HTMLInputElement | null} */ (document.querySelector(
    "#local-dropzone-input",
  ));

const uploadBtn = /** @type {HTMLButtonElement | null} */ (
  document.querySelector("#upload-btn")
);
const uploadIcon = /** @type {HTMLElement | null} */ (document.querySelector(
  "#upload-icon",
));
const uploadLabel = /** @type {HTMLElement | null} */ (document.querySelector(
  "#upload-label",
));
const uploadMenu = /** @type {HTMLElement | null} */ (document.querySelector(
  "#upload-menu",
));

const stopSyncBtn = /** @type {HTMLButtonElement | null} */ (
  document.querySelector("#stop-sync-btn")
);
const stopSyncLabel = /** @type {HTMLElement | null} */ (
  document.querySelector("#stop-sync-label")
);
const uploadIndicator = /** @type {HTMLElement | null} */ (
  document.querySelector("#upload-indicator")
);

const processBtn = /** @type {HTMLButtonElement | null} */ (
  document.querySelector("#process-btn")
);
const processIcon = /** @type {HTMLElement | null} */ (
  document.querySelector("#process-icon")
);
const processLabel = /** @type {HTMLElement | null} */ (
  document.querySelector("#process-label")
);

dropzoneInput?.addEventListener("change", async () => {
  const files = Array.from(dropzoneInput.files ?? []).filter((f) =>
    f.type.startsWith("audio/")
  );
  dropzoneInput.value = "";
  if (files.length === 0) return;
  await cacheFiles(files);
});

dropzone?.addEventListener("dragover", (e) => {
  e.preventDefault();
  dropzone.classList.add("dropzone--active");
});

dropzone?.addEventListener("dragleave", () => {
  dropzone.classList.remove("dropzone--active");
});

dropzone?.addEventListener("drop", async (e) => {
  e.preventDefault();
  dropzone.classList.remove("dropzone--active");

  const dragEvent = /** @type {DragEvent} */ (e);
  const items = Array.from(dragEvent.dataTransfer?.items ?? []);
  const files = await collectFiles(items);
  if (files.length === 0) return;

  await cacheFiles(files);
});

stopSyncBtn?.addEventListener("click", () => stopSyncing());

////////////////////////////////////////////
// OAUTH CALLBACK
////////////////////////////////////////////

// Detect the OAuth callback: if `?uploading=<scheme>` is in the URL and
// `#access_token=...` is in the hash, we just returned from the OAuth
// provider. Clean the URL (remove only the `uploading` param and the hash,
// preserving any other query parameters the loader needs) and resume the
// upload flow.
{
  const url = new URL(location.href);
  const uploadingScheme = url.searchParams.get("uploading");

  if (uploadingScheme) {
    const hashParams = new URLSearchParams(url.hash.slice(1));
    const accessToken = hashParams.get("access_token");

    // Clean URL: remove the `uploading` param and the hash, keep everything else.
    url.searchParams.delete("uploading");
    url.hash = "";
    history.replaceState({}, "", url);

    if (accessToken) {
      // Don't await — let the page render while the upload runs.
      resumeUpload(uploadingScheme, accessToken);
    } else {
      setError("Authorization failed. Please try again.");
    }
  }
}

////////////////////////////////////////////
// REACTIVE LISTS
////////////////////////////////////////////

// Recover filenames for tracks cached in previous sessions: the cached
// `File` blob retains its `.name` across IDB round-trips, so we read it back
// once on load and seed the in-memory map before the first render.
await (async () => {
  const tracks = await Output.data(outputOrchestrator.tracks);
  const ephemeralUris = tracks
    .filter((t) => t.uri.startsWith("ephemeral+cache://"))
    .map((t) => t.uri);

  await Promise.all(
    ephemeralUris.map(async (uri) => {
      if (ephemeralNames.has(uri)) return;
      const blob = await IDB.get(CACHE_KEY_PREFIX + uri);
      if (blob?.name) ephemeralNames.set(uri, blob.name);
    }),
  );
  namesVersion.value++;
})();

// Load persisted sync scheme.
{
  const scheme = await IDB.get(SYNC_SCHEME_KEY);
  if (scheme) syncScheme.value = scheme;
}

effect(() => {
  // Re-fire when ephemeralNames is updated (it's a plain Map, not a signal).
  namesVersion.get();
  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];

  // Local tracks: ephemeral cache tracks (files cached locally, not yet
  // uploaded to the cloud).
  const localEntries = tracks
    .filter((t) => t.uri.startsWith("ephemeral+cache://"))
    .map((t) => ({
      label: ephemeralNames.get(t.uri) ?? t.uri.split("://")[1],
      uri: t.uri,
    }))
    .sort((a, b) =>
      a.label.localeCompare(b.label, undefined, { sensitivity: "base" })
    );

  setLocalItems(
    localEntries.map(({ label, uri }) => ({
      name: label,
      onRemove: () => removeLocalEntry(uri),
    })),
  );

  // Remote tracks: tracks from the selected upload method's input only.
  // When no upload method is selected, no remote tracks are shown.
  const scheme = syncScheme.get();
  const remoteEntries = scheme
    ? tracks
        .filter(
          (t) =>
            t.uri.startsWith(scheme + "://") &&
            t.kind !== "placeholder",
        )
        .map((t) => ({
          label: trackLabel(t.uri),
          uri: t.uri,
        }))
        .sort((a, b) =>
          a.label.localeCompare(b.label, undefined, { sensitivity: "base" })
        )
    : [];

  setRemoteItems(
    remoteEntries.map(({ label, uri }) => ({
      name: label,
      onRemove: () => removeRemoteEntry(uri),
    })),
  );
});

////////////////////////////////////////////
// UPLOAD BUTTON / STOP SYNCING
////////////////////////////////////////////

// When not syncing: show the "Connect" button with a dropdown of available
// upload methods. When syncing: show a "Stop syncing" button that disconnects
// the upload method and stops auto-uploading.
effect(() => {
  if (!uploadBtn || !uploadIcon || !uploadLabel || !uploadMenu) return;
  if (!stopSyncBtn || !stopSyncLabel) return;

  const state = uploadState.get();
  const isBusy = state !== "idle";
  if (uploadIndicator) {
    uploadIndicator.hidden = !isBusy;
    uploadIndicator.title = state === "caching"
      ? "Processing local files …"
      : state === "uploading"
      ? "Uploading to cloud storage …"
      : "";
  }

  const syncing = syncScheme.get();

  if (syncing) {
    // Sync mode: hide upload button, show stop syncing button.
    uploadBtn.hidden = true;
    uploadMenu.hidePopover?.();
    stopSyncBtn.hidden = false;
    stopSyncLabel.textContent = `Stop syncing (${SCHEME_LABELS[syncing] ?? syncing})`;
    return;
  }

  // Idle mode: show upload button, hide stop syncing button.
  uploadBtn.hidden = false;
  uploadMenu.hidden = false;
  stopSyncBtn.hidden = true;

  const uploadComponents = uploadConfigurator.uploaders();
  const uploadEntries = Object.entries(uploadComponents);

  const canUpload = uploadEntries.length > 0;

  uploadBtn.disabled = isBusy || !canUpload;
  uploadIcon.className = "ph-fill ph-cloud-arrow-up";
  uploadLabel.textContent = isBusy
    ? "Uploading ..."
    : "Connect storage";

  litRender(
    html`
      ${uploadEntries.map(([scheme, element]) =>
        html`
          <button
            @click="${(/** @type {MouseEvent} */ e) => {
              /** @type {HTMLElement | null} */ (
                /** @type {HTMLElement} */ (e.currentTarget).closest("[popover]")
              )?.hidePopover();
              startUpload(scheme, element);
            }}"
          >
            <i class="ph-fill ph-cloud-arrow-up"></i>
            ${SCHEME_LABELS[scheme] ?? scheme}
          </button>
        `
      )}
    `,
    uploadMenu,
  );
});

////////////////////////////////////////////
// PROCESS BUTTON
////////////////////////////////////////////

effect(() => {
  if (!processBtn || !processIcon || !processLabel) return;

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

processBtn?.addEventListener("click", async () => {
  await Output.data(outputOrchestrator.tracks);
  await processOrchestrator.process();
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * Extracts a human-readable label from a track URI (the last path segment,
 * URL-decoded).
 *
 * @param {string} uri
 * @returns {string}
 */
function trackLabel(uri) {
  const withoutQuery = uri.split("?")[0];
  const parts = withoutQuery.split("/");
  const last = parts[parts.length - 1];
  return last ? safeDecodeURIComponent(last) : uri;
}

/**
 * Finds the first enabled (non-disabled) source URI for the given scheme.
 *
 * @param {string} scheme
 * @returns {string | null}
 */
function enabledSourceUri(scheme) {
  const sourcesRecord = sourcesOrchestrator.sources();
  const sources = (sourcesRecord[scheme] ?? []).filter(
    (s) => !sourcesOrchestrator.isDisabled(s.uri),
  );
  return sources.length > 0 ? sources[0].uri : null;
}

/** @param {string} uri */
async function removeLocalEntry(uri) {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: uri,
      tracks,
    });

    if (detachedTracks) {
      await outputOrchestrator.tracks.save(detachedTracks);
      ephemeralNames.delete(uri);
      namesVersion.value++;
    }
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to remove entry");
  }
}

/**
 * Deletes a remote track: first deletes the file from the cloud, then
 * detaches the track from the output.
 *
 * If the access token has expired, the delete is stored as pending and the
 * OAuth reconnection flow is triggered automatically. After reconnection,
 * `resumeUpload` picks up the pending delete and retries it.
 *
 * @param {string} uri
 */
async function removeRemoteEntry(uri) {
  setError(null);
  try {
    // Delete the file from the cloud first.
    await uploadConfigurator.delete(uri);

    // Then remove just this track from the output (not the entire source —
    // `inputConfigurator.detach` would remove all tracks for the account).
    const tracks = await Output.data(outputOrchestrator.tracks);
    const filteredTracks = tracks.filter((t) => t.uri !== uri);
    await outputOrchestrator.tracks.save(filteredTracks);

    // Re-list from the cloud and save the result. This ensures the output
    // is consistent with the cloud state. Without this, the process-tracks
    // orchestrator's `mergeById` (which uses stale `cachedTracks` from the
    // start of its run) can bring back the deleted track on the next
    // `process()` call.
    const listed = await inputConfigurator.list(filteredTracks);
    await outputOrchestrator.tracks.save(listed);
  } catch (err) {
    const msg = err instanceof Error ? err.message : "Failed to remove entry";
    if (msg.includes("expired")) {
      // Token expired — store the file path so the delete can be retried
      // after reconnection, then trigger OAuth.
      try {
        const path = new URL(uri).pathname;
        await IDB.set(PENDING_DELETE_KEY, path);
      } catch {
        // If path extraction fails, the user can retry manually after
        // reconnection.
      }
      reconnectSync();
      return;
    }
    setError(msg);
  }
}

/**
 * Uploads all ephemeral cache tracks to the selected source's cloud storage,
 * then removes them from the ephemeral cache (they're now in the cloud and
 * will be picked up by the matching input component's `list()` on the next
 * processing run).
 *
 * @param {string} sourceUri - The account/source URI to upload to.
 */
async function uploadTracks(sourceUri) {
  setError(null);
  uploadState.value = "uploading";

  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const ephemeralTracks = tracks.filter((t) =>
      t.uri.startsWith("ephemeral+cache://")
    );

    if (ephemeralTracks.length === 0) return;

    // Reconstruct Files from the IDB-cached blobs with their original names.
    // The cached blob is itself a `File` and retains its `.name` across IDB
    // round-trips, so it's a reliable fallback when `ephemeralNames` hasn't
    // been seeded yet (e.g. right after an OAuth redirect).
    const files = await Promise.all(
      ephemeralTracks.map(async (track) => {
        const blob = await IDB.get(CACHE_KEY_PREFIX + track.uri);
        const name = ephemeralNames.get(track.uri) ?? blob?.name ?? "audio";
        return new File([blob], name, { type: blob?.type ?? "audio/*" });
      }),
    );

    // Upload each file to the selected source's account.
    await Promise.all(
      files.map((file) => uploadConfigurator.upload({ file, uri: sourceUri })),
    );

    // Remove the ephemeral cache tracks (they're now in the cloud).
    const detachedTracks = await inputConfigurator.detach({
      fileUriOrScheme: SCHEME_EPHEMERAL_CACHE,
      tracks,
    });
    await outputOrchestrator.tracks.save(detachedTracks);

    // Clean up the filename map for the now-removed ephemeral tracks.
    for (const track of ephemeralTracks) {
      ephemeralNames.delete(track.uri);
    }
    namesVersion.value++;

    // List tracks from all input sources so the newly uploaded files appear.
    // We call `list` directly (instead of relying on the process-tracks
    // orchestrator) for reliability — the orchestrator may not be fully
    // initialised yet when this runs.
    const listed = await inputConfigurator.list(detachedTracks);
    await outputOrchestrator.tracks.save(listed);
  } catch (err) {
    const msg = err instanceof Error ? err.message : "Failed to upload tracks";
    if (msg.includes("expired")) {
      // Token expired — trigger reconnection. Ephemeral tracks are preserved
      // in IDB and will be uploaded automatically after reconnection.
      reconnectSync();
      return;
    }
    setError(msg);
  } finally {
    uploadState.value = "idle";
  }
}

/**
 * Uploads all ephemeral cache tracks to the sync source, then drains: if new
 * ephemeral tracks appeared during the upload (e.g. from concurrent drops),
 * uploads them too.
 */
async function syncUpload() {
  if (uploadState.get() === "uploading") return;
  if (!syncScheme.value) return;

  const sourceUri = enabledSourceUri(syncScheme.value);
  if (!sourceUri) return;

  await uploadTracks(sourceUri);

  // Drain: check if there are new ephemeral tracks (e.g. from concurrent
  // drops) and upload them too.
  if (syncScheme.value) {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const hasEphemeral = tracks.some((t) =>
      t.uri.startsWith("ephemeral+cache://")
    );
    if (hasEphemeral) {
      await syncUpload();
    }
  }
}

/**
 * Starts the sync flow for the selected upload component. Enters sync mode,
 * then uploads all ephemeral tracks. If an existing input source for this
 * scheme already exists (enabled or disabled), uploads directly to it
 * (re-enabling if necessary). Otherwise, triggers the OAuth flow (adding
 * `?uploading=<scheme>` to the URL so we can resume after the redirect).
 *
 * @param {string} scheme
 * @param {UploadElement} uploadElement
 */
async function startUpload(scheme, uploadElement) {
  setError(null);

  // Enter sync mode.
  syncScheme.value = scheme;
  await IDB.set(SYNC_SCHEME_KEY, scheme);

  const sourcesRecord = sourcesOrchestrator.sources();
  const allSources = sourcesRecord[scheme] ?? [];
  const enabledSources = allSources.filter(
    (s) => !sourcesOrchestrator.isDisabled(s.uri),
  );
  const disabledSources = allSources.filter((s) =>
    sourcesOrchestrator.isDisabled(s.uri),
  );

  // Check if an existing source's token is still valid before re-using it.
  // If the token has expired (consult returns "no"), fall through to the
  // OAuth flow to get a fresh token.
  if (enabledSources.length > 0) {
    const result = await uploadConfigurator.consult(enabledSources[0].uri);
    if (result.supported && result.consult !== "no") {
      // Token is valid (or inconclusive — be optimistic): upload directly.
      await uploadTracks(enabledSources[0].uri);
      return;
    }
  }

  if (disabledSources.length > 0) {
    const result = await uploadConfigurator.consult(disabledSources[0].uri);
    if (result.supported && result.consult !== "no") {
      // Token is valid — re-enable the source, then upload.
      await sourcesOrchestrator.toggle(disabledSources[0].uri);
      await uploadTracks(disabledSources[0].uri);
      return;
    }
  }

  // No existing source — authenticate first. Mark the URL so we can resume
  // the upload flow after the OAuth redirect.
  if (!uploadElement.authorize) {
    setError(`Upload component "${SCHEME_LABELS[scheme] ?? scheme}" does not support authorization.`);
    return;
  }

  const url = new URL(location.href);
  url.searchParams.set("uploading", scheme);
  history.replaceState({}, "", url);

  uploadElement.authorize();
  // Page will redirect to the OAuth provider.
}

/**
 * Resumes the sync flow after returning from the OAuth redirect. The access
 * token is in the URL hash; we use it to build a placeholder track (via the
 * upload component's `createSource`), upload files to that account, then save
 * the placeholder track so the matching input component lists the files.
 *
 * @param {string} scheme
 * @param {string} accessToken
 */
async function resumeUpload(scheme, accessToken) {
  setError(null);
  uploadState.value = "uploading";

  // Enter sync mode (persisted before the redirect, but set again for safety).
  syncScheme.value = scheme;
  await IDB.set(SYNC_SCHEME_KEY, scheme);

  try {
    /** @type {UploadElement | undefined} */
    const uploadElement = uploadConfigurator.uploaders()[scheme];
    if (!uploadElement) {
      setError(`Unsupported upload scheme: ${scheme}`);
      return;
    }

    const tracks = await Output.data(outputOrchestrator.tracks);
    const ephemeralTracks = tracks.filter((t) =>
      t.uri.startsWith("ephemeral+cache://")
    );

    // Build the placeholder track — its URI is the account URI we upload to.
    // Routed through the configurator so it reaches the correct upload
    // component by scheme.
    const placeholderTrack = await uploadConfigurator.createSource({
      scheme,
      accessToken,
      directoryPath: UPLOAD_DIRECTORY,
    });
    const uri = placeholderTrack.uri;

    // Upload ephemeral tracks if any.
    if (ephemeralTracks.length > 0) {
      // Reconstruct Files from the IDB-cached blobs with their original names.
      // The cached blob is itself a `File` and retains its `.name` across IDB
      // round-trips, so it's a reliable fallback when `ephemeralNames` hasn't
      // been seeded yet (e.g. right after an OAuth redirect).
      const files = await Promise.all(
        ephemeralTracks.map(async (track) => {
          const blob = await IDB.get(CACHE_KEY_PREFIX + track.uri);
          const name = ephemeralNames.get(track.uri) ?? blob?.name ?? "audio";
          return new File([blob], name, { type: blob?.type ?? "audio/*" });
        }),
      );

      // Upload each file to the account.
      await Promise.all(
        files.map((file) => uploadConfigurator.upload({ file, uri })),
      );
    }

    // Replace ALL existing tracks for this scheme (old placeholder + remote
    // tracks with an expired token) with the new placeholder. A new token
    // creates a different account ID, so old tracks won't be matched by a
    // fresh listing — they must be explicitly removed.
    const schemePrefix = scheme + "://";
    const tracksWithoutOldScheme = tracks.filter(
      (t) => !t.uri.startsWith(schemePrefix),
    );
    const tracksWithPlaceholder = [...tracksWithoutOldScheme, placeholderTrack];
    await outputOrchestrator.tracks.save(tracksWithPlaceholder);

    // Remove the ephemeral cache tracks if any were uploaded.
    let detachedTracks = tracksWithPlaceholder;
    if (ephemeralTracks.length > 0) {
      detachedTracks = await inputConfigurator.detach({
        fileUriOrScheme: SCHEME_EPHEMERAL_CACHE,
        tracks: tracksWithPlaceholder,
      });
      await outputOrchestrator.tracks.save(detachedTracks);

      // Clean up the filename map for the now-removed ephemeral tracks.
      for (const track of ephemeralTracks) {
        ephemeralNames.delete(track.uri);
      }
      namesVersion.value++;
    }

    // List tracks from all input sources so the newly uploaded files appear.
    // We call `list` directly (instead of relying on the process-tracks
    // orchestrator) for reliability — the orchestrator may not be fully
    // initialised yet when this runs.
    const listed = await inputConfigurator.list(detachedTracks);
    await outputOrchestrator.tracks.save(listed);

    // Retry a pending delete if one was stored (e.g. the user tried to delete
    // a track but the token had expired — after reconnection the track is
    // re-listed with a fresh token, so we retry the delete now).
    const pendingPath = await IDB.get(PENDING_DELETE_KEY);
    if (pendingPath) {
      await IDB.del(PENDING_DELETE_KEY);
      const target = listed.find((t) => {
        try {
          return new URL(t.uri).pathname === pendingPath;
        } catch {
          return false;
        }
      });
      if (target) {
        await uploadConfigurator.delete(target.uri);
        // Remove just this track from the output (not the entire source —
        // `inputConfigurator.detach` would remove all tracks for the account).
        const afterDelete = listed.filter((t) => t.uri !== target.uri);
        await outputOrchestrator.tracks.save(afterDelete);
      }
    }
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to upload tracks");
  } finally {
    uploadState.value = "idle";
  }
}

/**
 * Stops syncing: clears the sync state so new local tracks are no longer
 * auto-uploaded. Does NOT disable the input source or remove the remote
 * tracks — they stay in the output and remain visible (and deletable) in
 * the remote tracks list.
 */
async function stopSyncing() {
  setError(null);

  const scheme = syncScheme.value;
  if (!scheme) return;

  // Clear sync state. The input source stays enabled so remote tracks remain
  // visible and deletable.
  syncScheme.value = null;
  await IDB.del(SYNC_SCHEME_KEY);
}

/**
 * Triggers the OAuth flow to get a fresh access token for the current sync
 * scheme. Called automatically when an expired token is detected during a
 * delete or upload operation. Bypasses the `consult()` cache (which may
 * still report "yes" right after expiry) and goes straight to re-authorisation.
 */
function reconnectSync() {
  const scheme = syncScheme.value;
  if (!scheme) return;

  const uploadElement = uploadConfigurator.uploaders()[scheme];
  if (!uploadElement?.authorize) {
    setError(
      `Upload component "${SCHEME_LABELS[scheme] ?? scheme}" does not support reconnection.`,
    );
    return;
  }

  setError("Token expired. Reconnecting…");

  const url = new URL(location.href);
  url.searchParams.set("uploading", scheme);
  history.replaceState({}, "", url);

  uploadElement.authorize();
}

/**
 * @param {File[]} files
 */
async function cacheFiles(files) {
  setError(null);
  uploadState.value = "caching";
  try {
    const uris = await Promise.all(
      files.map((file) => inputConfigurator.cacheBlob(file)),
    );
    files.forEach((file, i) => {
      ephemeralNames.set(uris[i], file.name);
    });
    namesVersion.value++;
    const now = new Date().toISOString();
    const existingTracks = await Output.data(outputOrchestrator.tracks);
    const existingUris = new Set(existingTracks.map((t) => t.uri));
    const newUris = uris.filter((uri) => !existingUris.has(uri));
    await outputOrchestrator.tracks.save([
      ...existingTracks,
      ...newUris.map((uri) => {
        /** @type {Track} */
        const track = {
          $type: "sh.diffuse.output.track",
          id: TID.now(),
          createdAt: now,
          updatedAt: now,
          ephemeral: true,
          uri,
        };
        return track;
      }),
    ]);

    // If in sync mode, auto-upload the newly cached tracks.
    if (syncScheme.value) {
      await syncUpload();
    }
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to cache files");
  } finally {
    // Only reset if we're still in "caching" state — if syncUpload ran, it
    // manages its own upload state ("uploading" → "idle").
    if (uploadState.value === "caching") uploadState.value = "idle";
  }
}

/**
 * @param {DataTransferItem[]} items
 * @returns {Promise<File[]>}
 */
async function collectFiles(items) {
  const files = /** @type {File[]} */ ([]);

  await Promise.all(
    items.map(async (item) => {
      if (item.kind !== "file") return;

      const entry = item.webkitGetAsEntry?.();
      if (entry?.isDirectory) {
        const dirFiles = await readDirectoryEntry(
          /** @type {FileSystemDirectoryEntry} */ (entry),
        );
        files.push(...dirFiles);
      } else {
        const file = item.getAsFile();
        if (file?.type.startsWith("audio/")) files.push(file);
      }
    }),
  );

  return files;
}

/**
 * @param {FileSystemDirectoryEntry} dir
 * @returns {Promise<File[]>}
 */
async function readDirectoryEntry(dir) {
  const reader = dir.createReader();

  return new Promise((resolve, reject) => {
    /** @type {File[]} */
    const files = [];

    const readBatch = () => {
      reader.readEntries(async (entries) => {
        if (entries.length === 0) {
          resolve(files);
          return;
        }

        await Promise.all(
          entries.map(async (entry) => {
            if (entry.isDirectory) {
              const nested = await readDirectoryEntry(
                /** @type {FileSystemDirectoryEntry} */ (entry),
              );
              files.push(...nested);
            } else {
              const file = await new Promise(
                /** @param {(f: File) => void} res */
                (res, rej) =>
                  /** @type {FileSystemFileEntry} */ (entry).file(res, rej),
              );
              if (file.type.startsWith("audio/")) files.push(file);
            }
          }),
        );

        readBatch();
      }, reject);
    };

    readBatch();
  });
}

////////////////////////////////////////////
// AUTO-UPLOAD ON LOAD
////////////////////////////////////////////

// If we're in sync mode on page load (persisted from a previous session) and
// there are ephemeral tracks that haven't been uploaded yet, upload them now.
// Skip if resumeUpload is already handling it (detected via the OAuth callback).
{
  if (syncScheme.value && uploadState.get() !== "uploading") {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const hasEphemeral = tracks.some((t) =>
      t.uri.startsWith("ephemeral+cache://")
    );
    if (hasEphemeral) {
      syncUpload();
    }
  }
}

foundation.ready();
