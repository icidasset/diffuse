import { html, nothing, render as litRender } from "lit-html";

import foundation, { GROUP } from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

foundation.setup({ title: "Audio output" });

////////////////////////////////////////////
// CONSTANTS
////////////////////////////////////////////

/** Group every interface belongs to unless it overrides `?group=`. */
const DEFAULT_GROUP = "facets";

/**
 * Static `NAME` of the audio engine (`src/components/engine/audio/element.js`).
 * Its per-group state lives under `${AUDIO_ENGINE_NAME}/${group}/…` in
 * `localStorage`; the engine reads `${AUDIO_ENGINE_NAME}/${group}/sink` and
 * applies it to the shared `AudioContext`.
 */
const AUDIO_ENGINE_NAME = "diffuse/engine/audio";

/** @param {string} group */
const sinkKey = (group) => `${AUDIO_ENGINE_NAME}/${group}/sink`;

/**
 * Whether this browser can route Web Audio output to a chosen device.
 * `AudioContext.setSinkId` is Chromium-only for now.
 */
const SINK_SUPPORTED = typeof AudioContext !== "undefined" &&
  "setSinkId" in AudioContext.prototype;

////////////////////////////////////////////
// STATE
////////////////////////////////////////////

/** @type {ReturnType<typeof signal<string[]>>} */
const groups = signal(detectGroups());

/** @type {ReturnType<typeof signal<MediaDeviceInfo[]>>} */
const devices = signal(/** @type {MediaDeviceInfo[]} */ ([]));

////////////////////////////////////////////
// GROUP DETECTION
////////////////////////////////////////////

/**
 * Groups are not registered anywhere, so discover them from the per-group
 * state the audio engine (and friends) leave behind in `localStorage`. The
 * default group is always present.
 *
 * @returns {string[]}
 */
function detectGroups() {
  const found = new Set([DEFAULT_GROUP, GROUP]);

  for (let i = 0; i < localStorage.length; i++) {
    const key = localStorage.key(i);
    if (!key) continue;

    // Greedy `.+` so groups containing slashes survive intact.
    const match = /^diffuse\/engine\/audio\/(.+)\/(?:volume|sink)$/.exec(key);
    if (match) found.add(match[1]);
  }

  // Default group first, the rest alphabetically.
  return [...found].sort((a, b) => {
    if (a === DEFAULT_GROUP) return -1;
    if (b === DEFAULT_GROUP) return 1;
    return a.localeCompare(b);
  });
}

/** @param {string} group */
const sinkFor = (group) => localStorage.getItem(sinkKey(group)) ?? "";

/**
 * Persists the chosen device for a group and applies it.
 *
 * When this facet hosts the group's engine in the same document (unusual, but
 * possible for custom interfaces), a `storage` event wouldn't fire here, so
 * hand the change to the engine directly. Otherwise write `localStorage`; the
 * engine in another frame/tab applies it through its `storage` listener.
 *
 * @param {string} group
 * @param {string} sinkId
 */
function setGroupSink(group, sinkId) {
  if (group === GROUP) {
    const engine =
      /** @type {(HTMLElement & { setSink?: (id: string) => void }) | null} */ (
        document.querySelector("de-audio")
      );
    if (engine && typeof engine.setSink === "function") {
      engine.setSink(sinkId);
      return;
    }
  }

  if (sinkId) localStorage.setItem(sinkKey(group), sinkId);
  else localStorage.removeItem(sinkKey(group));
}

////////////////////////////////////////////
// DEVICES
////////////////////////////////////////////

async function refreshDevices() {
  const media = navigator.mediaDevices;

  if (!media?.enumerateDevices) {
    devices.set([]);
    return;
  }

  try {
    const all = await media.enumerateDevices();
    devices.set(all.filter((device) => device.kind === "audiooutput"));
  } catch (err) {
    console.warn("Failed to enumerate audio output devices.", err);
    devices.set([]);
  }
}

/**
 * Ask the browser for permission so output device labels (and `setSinkId`
 * itself, in some contexts) become available. Prefers the purpose-built
 * `selectAudioOutput` and falls back to a throwaway microphone grant.
 */
async function revealDeviceNames() {
  const media =
    /** @type {MediaDevices & { selectAudioOutput?: () => Promise<MediaDeviceInfo> }} */ (
      navigator.mediaDevices
    );
  if (!media) return;

  if (typeof media.selectAudioOutput === "function") {
    try {
      await media.selectAudioOutput();
      await refreshDevices();
      return;
    } catch (err) {
      // Fall through to the microphone fallback (e.g. the iframe lacks the
      // `speaker-selection` permission policy).
      console.debug("selectAudioOutput unavailable, falling back.", err);
    }
  }

  try {
    const stream = await media.getUserMedia({ audio: true });
    stream.getTracks().forEach((track) => track.stop());
  } catch (err) {
    console.warn("Could not gain access to audio output devices.", err);
  }

  await refreshDevices();
}

////////////////////////////////////////////
// RENDERING
////////////////////////////////////////////

const listEl = /** @type {HTMLElement} */ (
  document.querySelector("#groups-list")
);
const emptyEl = /** @type {HTMLElement} */ (document.querySelector("#empty"));
const supportNote = /** @type {HTMLElement} */ (
  document.querySelector("#support-note")
);

if (!SINK_SUPPORTED) {
  supportNote.hidden = false;
  supportNote.textContent =
    "This browser doesn't support choosing an audio output device, so playback " +
    "always uses the system default. Everything below is saved for browsers that do.";
}

/**
 * @param {MediaDeviceInfo[]} deviceList
 * @param {string} current
 * @returns {Array<{ value: string, label: string }>}
 */
function outputOptions(deviceList, current) {
  /** @type {Array<{ value: string, label: string }>} */
  const options = [{ value: "", label: "System default" }];

  deviceList.forEach((device, index) => {
    options.push({
      value: device.deviceId,
      label: device.label || `Output ${index + 1}`,
    });
  });

  // Keep a stored device selectable even when it's hidden (labels locked) or
  // currently unplugged, so the choice isn't silently reset.
  if (current && !deviceList.some((d) => d.deviceId === current)) {
    options.push({
      value: current,
      label: `Unavailable device (${current.slice(0, 8)}…)`,
    });
  }

  return options;
}

function render() {
  const deviceList = devices.get();
  const groupList = groups.get();

  emptyEl.hidden = deviceList.length > 0;

  litRender(
    html`
      ${groupList.map((group) => {
        const current = sinkFor(group);
        const options = outputOptions(deviceList, current);

        return html`
          <li class="output-item">
            <div class="output-item__info">
              ${group === DEFAULT_GROUP
                ? html`<span class="badge badge--brand" style="display: block; margin-bottom: var(--space-2xs);">Default</span>`
                : html`<span class="output-item__name">${group}</span>`}
            </div>
            <select
              data-group="${group}"
              aria-label="Audio output device for group ${group}"
              @change="${onSelectChange}"
            >
              ${options.map((option) =>
                html`<option value="${option.value}">${option.label}</option>`
              )}
            </select>
            ${current && !deviceList.some((d) => d.deviceId === current)
              ? html`
                <span class="output-item__hint">
                  This device isn't available right now; playback falls back to
                  the system default until it's back.
                </span>
              `
              : nothing}
          </li>
        `;
      })}
    `,
    listEl,
  );

  // Reflect stored values onto the selects. Setting `selected` on the options
  // during render is unreliable for dynamically rendered `<select>`s.
  listEl.querySelectorAll("select").forEach((select) => {
    const group = select.getAttribute("data-group");
    if (group) select.value = sinkFor(group);
  });
}

/** @param {Event} event */
function onSelectChange(event) {
  const select = /** @type {HTMLSelectElement} */ (event.currentTarget);
  const group = select.getAttribute("data-group");
  if (!group) return;
  setGroupSink(group, select.value);
}

////////////////////////////////////////////
// EVENTS
////////////////////////////////////////////

effect(() => {
  devices.get();
  groups.get();
  render();
});

const refreshBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#refresh-btn"));
const revealBtn =
  /** @type {HTMLButtonElement} */ (document.querySelector("#reveal-btn"));

refreshBtn.addEventListener("click", () => {
  refreshDevices();
});

revealBtn.addEventListener("click", () => {
  revealDeviceNames();
});

// Re-detect groups and re-render when another frame/tab writes per-group state.
globalThis.addEventListener("storage", (/** @type {StorageEvent} */ event) => {
  if (
    event.key !== null && !/^diffuse\/engine\/audio\/.+\/(?:volume|sink)$/.test(
      event.key,
    )
  ) {
    return;
  }
  groups.set(detectGroups());
  render();
});

navigator.mediaDevices?.addEventListener?.("devicechange", () => {
  refreshDevices();
});

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

await refreshDevices();

foundation.ready();
