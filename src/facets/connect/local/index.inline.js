import * as TID from "@atcute/tid";
import { html, nothing } from "lit-html";

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
  `,

  rightContent: html`
    <label class="dropzone" id="local-dropzone">
      <input id="local-dropzone-input" type="file" accept="audio/*" multiple hidden />
      <i class="ph-bold ph-upload-simple"></i>
      <span>Drop or click to select files</span>
    </label>

    <hr id="local-ephemeral-divider" hidden>
    <div id="local-ephemeral-row" class="button-row" hidden>
      <button
        id="local-clear-ephemeral-btn"
        class="button--danger button--small"
        style="width: 100%"
      >
        <i class="ph-bold ph-trash"></i>
        Clear files
      </button>
    </div>

    ${supported
      ? html`
        <hr>

        <div class="button-row">
          <button id="local-add-dir-btn">
            <i class="ph-fill ph-folder-open"></i>
            Add directory
          </button>
          <button id="local-add-files-btn">
            <i class="ph-fill ph-music-notes"></i>
            Add files
          </button>
        </div>
      `
      : nothing}
  `,

  formFields: html``,
  onSubmit: async () => {},
});

document
  .querySelector("#local-add-dir-btn")
  ?.addEventListener("click", () => addDirectory());

document
  .querySelector("#local-add-files-btn")
  ?.addEventListener("click", () => addFiles());

document
  .querySelector("#local-clear-ephemeral-btn")
  ?.addEventListener("click", () => clearEphemeral());

const dropzone = document.querySelector("#local-dropzone");
const dropzoneInput =
  /** @type {HTMLInputElement | null} */ (document.querySelector(
    "#local-dropzone-input",
  ));

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

////////////////////////////////////////////
// REACTIVE LIST
////////////////////////////////////////////

const ephemeralDivider =
  /** @type {HTMLElement | null} */ (document.querySelector(
    "#local-ephemeral-divider",
  ));
const ephemeralRow =
  /** @type {HTMLElement | null} */ (document.querySelector(
    "#local-ephemeral-row",
  ));

effect(() => {
  const tracksCol = outputOrchestrator.tracks.collection();
  const tracks = tracksCol.state === "loaded" ? tracksCol.data : [];
  const hasEphemeral = tracks.some((t) =>
    t.uri.startsWith("ephemeral+cache://")
  );

  if (ephemeralDivider) ephemeralDivider.hidden = !hasEphemeral;
  if (ephemeralRow) ephemeralRow.hidden = !hasEphemeral;

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

async function clearEphemeral() {
  setError(null);
  try {
    const tracks = await Output.data(outputOrchestrator.tracks);
    const ephemeralUris = tracks
      .filter((t) => t.uri.startsWith("ephemeral+cache://"))
      .map((t) => t.uri);

    await inputConfigurator.removeFromCache(ephemeralUris);
    await outputOrchestrator.tracks.save(
      tracks.filter((t) => !t.uri.startsWith("ephemeral+cache://")),
    );
  } catch (err) {
    setError(
      err instanceof Error ? err.message : "Failed to clear cached files",
    );
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

/**
 * @param {File[]} files
 */
async function cacheFiles(files) {
  setError(null);
  try {
    const uris = await Promise.all(
      files.map((file) => inputConfigurator.cacheBlob(file)),
    );
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
  } catch (err) {
    setError(err instanceof Error ? err.message : "Failed to cache files");
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
// 🚀
////////////////////////////////////////////

foundation.ready();
