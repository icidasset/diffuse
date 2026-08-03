import { html, nothing, render as litRender } from "lit-html";

import * as Output from "~/common/output.js";
import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

/**
 * @typedef {{ id: string, label: string, element: import("@specs/components/output/types.d.ts").OutputElement }} OutputOption
 */

foundation.setup({ title: "Your data | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const outputOrchestrator = await foundation.orchestrator.output();
await customElements.whenDefined(outputOrchestrator.localName);

// The output configurator can't find custom outputs (added by the
// output-bundle prelude) during its initial connectedCallback, so it
// re-loads the selected output via `loadSelected()` which is async.
// Wait for that to settle before rendering the UI, otherwise
// `selected()` is still null here and the active-method badge would be
// wrong.
if (outputOrchestrator.hasSelected() && !outputOrchestrator.selected()) {
  await new Promise((resolve) => {
    const stop = effect(() => {
      if (outputOrchestrator.selected()) {
        stop();
        resolve(undefined);
      }
    });
  });
}

/** @type {Record<string, string>} */
const ICONS = {
  "Local": "database",
  "AT Protocol": "at",
  "S3": "hard-drives",
  "Dropbox": "cloud",
};

/** @type {Record<string, string>} */
const CONNECT_URLS = {
  "AT Protocol": "facets/connect/atproto-passkey/index.html",
  "S3": "facets/connect/s3/index.html",
  "Dropbox": "facets/connect/dropbox/index.html",
};

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {import("~/common/signal.d.ts").Signal<OutputOption[]>} */
const options = signal(/** @type {OutputOption[]} */ ([]));

/**
 * Incremented when a custom element for an activated method finishes
 * upgrading. The render effects read this so they re-run and can then
 * access `element.tracks.collection()` (which isn't available before
 * the element is upgraded).
 */
const refresh = signal(0);

options.value = await outputOrchestrator.options();

const defaultId = outputOrchestrator.outputConfigurator.getAttribute(
  "default",
);

////////////////////////////////////////////
// UI ELEMENTS
////////////////////////////////////////////

const list =
  /** @type {HTMLElement} */ (document.querySelector("#userdata-list"));
const emptyEl =
  /** @type {HTMLElement} */ (document.querySelector("#userdata-empty"));
const migrateDivider =
  /** @type {HTMLElement} */ (document.querySelector("#migrate-divider"));
const migrateSection =
  /** @type {HTMLElement} */ (document.querySelector("#migrate-section"));
const migrateFrom =
  /** @type {HTMLSelectElement} */ (document.querySelector("#migrate-from"));
const migrateTo =
  /** @type {HTMLSelectElement} */ (document.querySelector("#migrate-to"));
const migrateBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#migrate-btn"));
const migrateStatus =
  /** @type {HTMLElement} */ (document.querySelector("#migrate-status"));

////////////////////////////////////////////
// ELEMENT UPGRADE WATCHER
////////////////////////////////////////////

/**
 * Tracks which custom element names have already been scheduled for a
 * `whenDefined` callback, so we don't schedule duplicates.
 *
 * @type {Set<string>}
 */
const scheduled = new Set();

/**
 * When a method is activated, the output-bundle lazily imports its
 * custom element. We schedule a `whenDefined` callback for each newly
 * activated method and bump `refresh` when it resolves, so the render
 * effect can then safely read the element's collection signals.
 */
effect(() => {
  const opts = options.get();
  const activatedSet = outputOrchestrator.activated();

  for (const opt of opts) {
    const name = opt.element.localName;
    if (
      activatedSet.has(opt.id) &&
      !customElements.get(name) &&
      !scheduled.has(name)
    ) {
      scheduled.add(name);
      customElements.whenDefined(name).then(() => {
        refresh.value++;
      });
    }
  }
});

////////////////////////////////////////////
// METHODS LIST
////////////////////////////////////////////

/**
 * @param {OutputOption} opt
 * @param {Set<string>} activatedSet
 * @returns {number | null} - track count, or null if not yet loaded
 */
function getTrackCount(opt, activatedSet) {
  if (!activatedSet.has(opt.id)) return null;
  const el = opt.element;
  if (!el || typeof el.tracks !== "object" || !el.tracks) return null;
  try {
    const col = el.tracks.collection();
    if (col.state === "loaded") return col.data.length;
    return null;
  } catch {
    return null;
  }
}

effect(() => {
  const opts = options.get();
  const selected = outputOrchestrator.selected();
  const activatedSet = outputOrchestrator.activated();
  refresh.get();

  if (opts.length === 0) {
    list.hidden = true;
    emptyEl.hidden = false;
    return;
  }

  list.hidden = false;
  emptyEl.hidden = true;

  const activeId = selected?.id ?? defaultId;

  litRender(
    html`
      ${opts.map((opt) => {
        const isActive = opt.id === activeId;
        const isActivated = activatedSet.has(opt.id);
        const trackCount = getTrackCount(opt, activatedSet);
        const icon = ICONS[opt.label] ?? "database";

        /** @type {string | typeof nothing} */
        let detail;
        if (!isActivated) {
          detail = "Not configured";
        } else if (trackCount !== null) {
          detail = `${trackCount} track${trackCount === 1 ? "" : "s"}`;
        } else {
          detail = "Loading…";
        }

        return html`
          <li
            class="userdata-item ${isActive
              ? "userdata-item--active"
              : ""}"
          >
            <i class="ph-fill ph-${icon} userdata-item__icon"></i>
            <div class="userdata-item__info">
              <span class="userdata-item__name">
                ${opt.label}
                ${isActive
                  ? html`<span class="userdata-item__badge">Active</span>`
                  : nothing}
              </span>
              <span class="userdata-item__detail">${detail}</span>
            </div>
            ${isActive
              ? nothing
              : isActivated
              ? html`
                <button
                  class="button--outlined button--small"
                  title="Select this method"
                  @click="${() => outputOrchestrator.select(opt.id)}"
                >
                  <i class="ph-fill ph-check"></i>
                  Select
                </button>
              `
              : CONNECT_URLS[opt.label]
              ? html`
                <a
                  class="button button--outlined button--small"
                  title="Configure this method"
                  href="./l/?path=${CONNECT_URLS[opt.label]}"
                >
                  <i class="ph-fill ph-gear"></i>
                  Configure
                </a>
              `
              : nothing}
          </li>
        `;
      })}
    `,
    list,
  );
});

////////////////////////////////////////////
// MIGRATION DROPDOWNS
////////////////////////////////////////////

/**
 * @param {HTMLSelectElement} select
 * @param {OutputOption[]} opts
 * @param {string} prevValue
 */
function populateDropdown(select, opts, prevValue) {
  select.innerHTML = "";

  if (opts.length === 0) {
    select.disabled = true;
    return;
  }

  select.disabled = false;
  for (const opt of opts) {
    const option = document.createElement("option");
    option.value = opt.id;
    option.textContent = opt.label;
    select.append(option);
  }

  const stillAvailable = opts.some((o) => o.id === prevValue);
  select.value = stillAvailable ? prevValue : (opts[0]?.id ?? "");
}

effect(() => {
  const opts = options.get();
  const activatedSet = outputOrchestrator.activated();
  refresh.get();

  const activatedOpts = opts.filter((opt) =>
    activatedSet.has(opt.id) && customElements.get(opt.element.localName)
  );

  const showMigrate = activatedOpts.length >= 2;
  migrateDivider.hidden = !showMigrate;
  migrateSection.hidden = !showMigrate;

  if (!showMigrate) return;

  const prevFrom = migrateFrom.value;
  const prevTo = migrateTo.value;

  populateDropdown(migrateFrom, activatedOpts, prevFrom);
  populateDropdown(migrateTo, activatedOpts, prevTo);

  updateMigrateBtn();
});

function updateMigrateBtn() {
  const fromId = migrateFrom.value;
  const toId = migrateTo.value;
  migrateBtn.disabled = !fromId || !toId || fromId === toId;
}

migrateFrom.addEventListener("change", updateMigrateBtn);
migrateTo.addEventListener("change", updateMigrateBtn);

////////////////////////////////////////////
// MIGRATION ACTION
////////////////////////////////////////////

/**
 * @param {string} message
 * @param {"success" | "error" | null} type
 */
function showMigrateStatus(message, type) {
  migrateStatus.textContent = message;
  migrateStatus.className = type ? `status status--${type}` : "status";
  migrateStatus.hidden = false;
}

/**
 * @param {OutputOption} fromOpt
 * @returns {Promise<{ tracks: any[], playlistItems: any[], facets: any[], settings: any[] }>}
 */
async function readAllData(fromOpt) {
  const [tracks, playlistItems, facets, settings] = await Promise.all([
    Output.data(fromOpt.element.tracks),
    Output.data(fromOpt.element.playlistItems),
    Output.data(fromOpt.element.facets),
    Output.data(fromOpt.element.settings),
  ]);
  return { tracks, playlistItems, facets, settings };
}

/**
 * @param {OutputOption} toOpt
 * @param {{ tracks: any[], playlistItems: any[], facets: any[], settings: any[] }} data
 */
async function writeAllData(toOpt, data) {
  await Promise.all([
    toOpt.element.tracks.save(data.tracks),
    toOpt.element.playlistItems.save(data.playlistItems),
    toOpt.element.facets.save(data.facets),
    toOpt.element.settings.save(data.settings),
  ]);
}

migrateBtn.addEventListener("click", async () => {
  const fromId = migrateFrom.value;
  const toId = migrateTo.value;
  if (!fromId || !toId || fromId === toId) return;

  const fromOpt = options.get().find((o) => o.id === fromId);
  const toOpt = options.get().find((o) => o.id === toId);
  if (!fromOpt || !toOpt) return;

  if (!fromOpt.element?.tracks || !toOpt.element?.tracks) {
    showMigrateStatus("One or both methods are not ready.", "error");
    return;
  }

  if (
    !confirm(
      `This will replace all data in "${toOpt.label}" with data from "${fromOpt.label}". Continue?`,
    )
  ) {
    return;
  }

  const btnSpan =
    /** @type {HTMLElement} */ (migrateBtn.querySelector("span"));
  const originalLabel = btnSpan.textContent;
  migrateBtn.disabled = true;
  btnSpan.textContent = "Migrating…";
  showMigrateStatus("Reading data from " + fromOpt.label + "…", null);

  try {
    const data = await readAllData(fromOpt);
    showMigrateStatus("Writing to " + toOpt.label + "…", null);
    await writeAllData(toOpt, data);

    showMigrateStatus(
      `Migrated ${data.tracks.length} track${data.tracks.length === 1 ? "" : "s"}, ${data.playlistItems.length} playlist item${data.playlistItems.length === 1 ? "" : "s"}, ${data.facets.length} facet${data.facets.length === 1 ? "" : "s"}, and ${data.settings.length} setting${data.settings.length === 1 ? "" : "s"} from ${fromOpt.label} to ${toOpt.label}.`,
      "success",
    );
  } catch (err) {
    console.error("Migration failed:", err);
    showMigrateStatus(
      `Migration failed: ${/** @type {Error} */ (err).message}`,
      "error",
    );
  } finally {
    btnSpan.textContent = originalLabel;
    updateMigrateBtn();
  }
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
