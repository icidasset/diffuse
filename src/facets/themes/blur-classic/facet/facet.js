import foundation from "~/common/foundation.js";
import { data } from "~/common/output.js";

// Move #bg-overlay to document.body so it's not inside #container.
// #container fades in via an opacity transition (0→1), which creates an
// isolated stacking context while opacity < 1. That breaks mix-blend-mode
// on the overlay: it blends against transparent instead of the dark page
// background, causing a flash of regular image colors until opacity reaches 1.
const overlayEl = document.querySelector("#bg-overlay");
if (overlayEl) document.body.appendChild(overlayEl);

// Set doc title
foundation.setup({ title: "Blur Classic | Diffuse" });

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

await foundation.engine.queue();
await foundation.engine.repeatShuffle();
await foundation.engine.scope();
await foundation.orchestrator.scopedTracks();

await foundation.orchestrator.sources();
await foundation.orchestrator.processTracks({ disableWhenReady: true });
await foundation.orchestrator.queueAudio();
await foundation.orchestrator.controller();
await foundation.orchestrator.mediaSession();
await foundation.orchestrator.artwork();
await foundation.orchestrator.coverGroups();
await foundation.orchestrator.favourites();
await foundation.configurator.input();

await import("~/facets/themes/blur-classic/controller/element.js");
await import("~/facets/themes/blur/browser/element.js");

const groupLabel = foundation.GROUP === "facets" ? "Deck A" : foundation.GROUP;
const controller = document.querySelector("db-blur-classic-controller");

controller?.setAttribute("group", foundation.GROUP);
controller?.setAttribute("group-label", groupLabel);

document.querySelector("db-browser")?.setAttribute("group", foundation.GROUP);

////////////////////////////////////////////
// BACKGROUND SETTINGS
////////////////////////////////////////////

const BACKGROUND_KEY = "sh.diffuse.theme.blur.background";
const BACKGROUND_COLOR_KEY = "sh.diffuse.theme.blur.background-color";
const BACKGROUND_MIX_KEY = "sh.diffuse.theme.blur.background-mix";
const BG_IMAGE_COUNT = 30;

const output = await foundation.orchestrator.output();
await data(output.settings);

// Apply stored image and color before fading in (with defaults for first run)
const storedBg = getSettingValue(BACKGROUND_KEY);
const storedBgColor = getSettingValue(BACKGROUND_COLOR_KEY);
const storedBgMix = getSettingValue(BACKGROUND_MIX_KEY);

const activeBg = storedBg ?? "builtin:7";
const activeMix = storedBgMix !== null ? storedBgMix === "true" : false;

applyBackgroundMix(activeMix);
if (storedBgColor) applyBackgroundColor(storedBgColor);
applyBackgroundImage(activeBg);

////////////////////////////////////////////
// SHORTCUTS
////////////////////////////////////////////

document.querySelector("#btn-new-deck")?.addEventListener("click", async () => {
  const state = await navigator.locks.query();
  const held = (state.held ?? []).flatMap((l) => l.name ? [l.name] : []);

  let nextGroup;

  if (!held.some((n) => n.includes("/Deck B"))) {
    nextGroup = "Deck B";
  } else if (!held.some((n) => n.includes("/Deck C"))) {
    nextGroup = "Deck C";
  } else {
    return;
  }

  const url = new URL(document.location.href);
  url.searchParams.set("group", nextGroup);
  window.open(url.toString(), "_blank");
});

const settingsBtn = document.querySelector("#btn-settings");
const settingsPanel =
  /** @type {HTMLElement | null} */ (document.querySelector("#settings-panel"));
const settingsBackdrop = document.querySelector("#settings-backdrop");
const browserEl = document.querySelector("db-browser");

let settingsOpen = false;

function positionSettingsPanel() {
  if (!browserEl || !settingsPanel) return;
  const rect = browserEl.getBoundingClientRect();
  settingsPanel.style.top = `${rect.top}px`;
  settingsPanel.style.left = `${rect.left}px`;
  settingsPanel.style.width = `${rect.width}px`;
  settingsPanel.style.height = `${rect.height}px`;
}

function openSettings() {
  settingsOpen = true;
  positionSettingsPanel();
  settingsBackdrop?.classList.add("settings-open");
  settingsPanel?.classList.add("settings-open");
  settingsBtn?.setAttribute("data-active", "t");
}

function closeSettings() {
  settingsOpen = false;
  settingsBackdrop?.classList.remove("settings-open");
  settingsPanel?.classList.remove("settings-open");
  settingsBtn?.setAttribute("data-active", "f");
}

settingsBtn?.addEventListener("click", () => {
  if (settingsOpen) closeSettings();
  else openSettings();
});

settingsBackdrop?.addEventListener("click", closeSettings);

document.querySelector("#settings-close")?.addEventListener(
  "click",
  closeSettings,
);

document.addEventListener("keydown", (e) => {
  if (e.key === "Escape" && settingsOpen) closeSettings();
});

window.addEventListener("resize", () => {
  if (settingsOpen) positionSettingsPanel();
});

////////////////////////////////////////////
// SETTINGS PANEL
////////////////////////////////////////////

// Populate background image grid
const bgGrid = document.querySelector("#bg-images");

if (bgGrid) {
  for (let i = 1; i <= BG_IMAGE_COUNT; i++) {
    const btn = document.createElement("button");
    btn.className = "bg-thumb";
    btn.dataset.value = `builtin:${i}`;
    btn.title = `Background ${i}`;

    const img = document.createElement("img");
    img.src = `images/background/thumbnails/${i}.jpg`;
    img.alt = `Background ${i}`;
    img.loading = "lazy";

    const check = document.createElement("i");
    check.className = "ph-bold ph-check bg-thumb-check";

    btn.append(img, check);
    btn.addEventListener("click", async () => {
      const value = `builtin:${i}`;
      await saveSetting(BACKGROUND_KEY, value);
      await applyBackgroundImage(value);
      updateImageSelected(value);
    });

    bgGrid.append(btn);
  }
}

// Reflect current selections in the UI
updateImageSelected(activeBg);
updateColorSelected(storedBgColor);
updateMixSelected(activeMix);

// Image: None button
document.querySelector("#bg-none-btn")?.addEventListener("click", async () => {
  await saveSetting(BACKGROUND_KEY, "");
  await applyBackgroundImage("");
  updateImageSelected("");
});

// Image: URL toggle
document.querySelector("#bg-custom-btn")?.addEventListener("click", () => {
  const row = /** @type {HTMLElement | null} */ (
    document.querySelector("#bg-url-row")
  );
  if (row) row.hidden = !row.hidden;
  document.querySelector("#bg-custom-btn")?.toggleAttribute(
    "data-selected",
    row ? !row.hidden : false,
  );
});

// Image: apply custom URL
document.querySelector("#bg-url-apply")?.addEventListener("click", async () => {
  const input = /** @type {HTMLInputElement | null} */ (
    document.querySelector("#bg-url-input")
  );
  const url = input?.value?.trim();
  if (!url) return;
  const value = `url:${url}`;
  await saveSetting(BACKGROUND_KEY, value);
  await applyBackgroundImage(value);
  updateImageSelected(value);
});

// Color: picker — label wraps the input, clicking opens the native picker
document.querySelector("#bg-color-picker")?.addEventListener(
  "change",
  async (e) => {
    const color = /** @type {HTMLInputElement} */ (e.target).value;
    await saveSetting(BACKGROUND_COLOR_KEY, color);
    applyBackgroundColor(color);
    updateColorSelected(color);
  },
);

// Color: clear button
document.querySelector("#bg-color-clear-btn")?.addEventListener(
  "click",
  async () => {
    await saveSetting(BACKGROUND_COLOR_KEY, "");
    applyBackgroundColor("");
    updateColorSelected(null);
  },
);

// Mix: toggle
document.querySelector("#bg-mix-btn")?.addEventListener("click", async () => {
  const isMixed = !(document.querySelector("#bg-overlay")?.classList.contains(
    "bg-overlay--no-mix",
  ) ?? false);
  const next = !isMixed;
  applyBackgroundMix(next);
  updateMixSelected(next);
  await saveSetting(BACKGROUND_MIX_KEY, next ? "true" : "false");
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();

////////////////////////////////////////////
// 🛠️ HELPERS
////////////////////////////////////////////

/**
 * Returns the stored value for a settings key, or null if absent.
 * @param {string} key
 * @returns {string | null}
 */
function getSettingValue(key) {
  const col = output.settings.collection();
  if (col.state !== "loaded") return null;
  return col.data.find((s) => s.key === key)?.value ?? null;
}

/**
 * Persist a value to a settings key. Pass "" to remove the setting.
 * @param {string} key
 * @param {string} value
 */
async function saveSetting(key, value) {
  const col = output.settings.collection();
  if (col.state !== "loaded") return;

  const settings = col.data;
  const existing = settings.find((s) => s.key === key);

  /** @type {import("~/definitions/types.d.ts").Setting[]} */
  let updated;

  if (!value) {
    updated = settings.filter((s) => s.key !== key);
  } else if (existing) {
    updated = settings.map((s) => s.key === key ? { ...s, value } : s);
  } else {
    updated = [
      ...settings,
      {
        $type: /** @type {"sh.diffuse.output.setting"} */ (
          "sh.diffuse.output.setting"
        ),
        id: crypto.randomUUID(),
        key,
        value,
      },
    ];
  }

  await output.settings.save(updated);
}

/**
 * Apply a background image value to #bg-overlay. Preloads before fading in.
 * @param {string} value  builtin:N | url:... | "" for none
 */
async function applyBackgroundImage(value) {
  const overlay = /** @type {HTMLElement | null} */ (
    document.querySelector("#bg-overlay")
  );

  if (!overlay) return;

  const wasVisible = overlay.classList.contains("bg-overlay--visible");
  overlay.classList.remove("bg-overlay--visible");

  // Wait for the fade-out to finish before swapping the image, so that
  // backgroundImage never changes while the overlay is partially opaque
  // (which would recreate the GPU layer and drop the blend mode for a frame).
  // On initial load the overlay is already transparent so no wait is needed.
  if (wasVisible) {
    await new Promise((resolve) => {
      overlay.addEventListener("transitionend", resolve, { once: true });
    });
  }

  if (!value) {
    overlay.style.backgroundImage = "";
    return;
  }

  let imageUrl = "";
  if (value.startsWith("builtin:")) {
    imageUrl = `images/background/${value.slice(8)}.jpg`;
  } else if (value.startsWith("url:")) {
    imageUrl = value.slice(4);
  }

  if (imageUrl) {
    await new Promise((resolve) => {
      const img = new Image();
      img.onload = resolve;
      img.onerror = resolve;
      img.src = imageUrl;
    });

    overlay.style.backgroundImage = `url('${imageUrl.replace(/'/g, "\\'")}')`;
    overlay.style.backgroundPosition = value.startsWith("builtin:")
      ? backgroundPositioning(`${value.slice(8)}.jpg`)
      : "";

    await new Promise((resolve) => {
      requestAnimationFrame(() => resolve(undefined));
    });

    overlay.classList.add("bg-overlay--visible");
  }
}

/**
 * Returns the background-position value for a given image filename.
 * @param {string} filename
 * @returns {string}
 */
function backgroundPositioning(filename) {
  switch (filename) {
    case "2.jpg":
      return "center 68%";
    case "3.jpg":
      return "center 30%";
    case "4.jpg":
      return "center 96.125%";
    case "6.jpg":
      return "center 40%";
    case "11.jpg":
      return "center 67.25%";
    case "19.jpg":
      return "center 13%";
    case "20.jpg":
      return "center 39.75%";
    case "21.jpg":
      return "center 52.5%";
    case "22.jpg":
      return "center top";
    case "23.jpg":
      return "center 92.5%";
    case "24.jpg":
      return "center top";
    case "25.jpg":
      return "center 50%";
    case "27.jpg":
      return "center top";
    default:
      return "center bottom";
  }
}

/**
 * Apply a background color value as the page background color.
 * @param {string | null} color  CSS color string, or null/empty to clear
 */
function applyBackgroundColor(color) {
  if (color) {
    document.documentElement.style.setProperty("--facet-bg-color", color);
  } else {
    document.documentElement.style.removeProperty("--facet-bg-color");
  }
}

/**
 * Highlight the active image selection in the settings panel.
 * @param {string} value
 */
function updateImageSelected(value) {
  document.querySelectorAll(".bg-thumb, #bg-none-btn, #bg-custom-btn").forEach(
    (el) => el.removeAttribute("data-selected"),
  );

  if (!value) {
    document.querySelector("#bg-none-btn")?.setAttribute("data-selected", "");
  } else if (value.startsWith("builtin:")) {
    document
      .querySelector(`.bg-thumb[data-value="${value}"]`)
      ?.setAttribute("data-selected", "");
  } else if (value.startsWith("url:")) {
    const input = /** @type {HTMLInputElement | null} */ (
      document.querySelector("#bg-url-input")
    );
    if (input) input.value = value.slice(4);
    const row = /** @type {HTMLElement | null} */ (
      document.querySelector("#bg-url-row")
    );
    if (row) row.hidden = false;
    document.querySelector("#bg-custom-btn")?.setAttribute("data-selected", "");
  }
}

/**
 * Toggle mix-blend-mode on #bg-overlay.
 * @param {boolean} enabled
 */
function applyBackgroundMix(enabled) {
  document.querySelector("#bg-overlay")?.classList.toggle(
    "bg-overlay--no-mix",
    !enabled,
  );
}

/**
 * Reflect the mix toggle state in the settings panel.
 * @param {boolean} enabled
 */
function updateMixSelected(enabled) {
  const btn = document.querySelector("#bg-mix-btn");
  btn?.toggleAttribute("data-selected", enabled);
}

/**
 * Highlight the active color selection and update the swatch.
 * @param {string | null} color
 */
function updateColorSelected(color) {
  const label = /** @type {HTMLElement | null} */ (
    document.querySelector("#bg-color-label")
  );
  const picker = /** @type {HTMLInputElement | null} */ (
    document.querySelector("#bg-color-picker")
  );

  if (color) {
    label?.setAttribute("data-selected", "");
    if (label) label.style.backgroundColor = color;
    if (picker) picker.value = color;
  } else {
    label?.removeAttribute("data-selected");
    if (label) label.style.backgroundColor = "";
  }
}
