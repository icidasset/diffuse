import { html, nothing, render as litRender } from "lit-html";

import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

import {
  CLASS as ATProtoPublish,
  MARKER,
  NAME as PUBLISH_NAME,
} from "~/components/supplement/atproto-publish/element.js";

/**
 * @import {ATProtoPublishElement} from "@specs/components/supplement/atproto-publish/types.d.ts"
 * @typedef {{ did: string, handle: string, displayName?: string, avatar?: string }} HandleSuggestion
 */

foundation.setup({ title: "Atmosphere" });

////////////////////////////////////////////
// ATMOSPHERE ACCOUNT
////////////////////////////////////////////

// The publish element owns the shared Atmosphere OAuth session. Further
// Atmosphere-powered features (e.g. spaces) can reuse the account section
// below and add their own section to the right column.
const outputOrchestrator = await foundation.orchestrator.output();
await customElements.whenDefined(outputOrchestrator.localName);

let existing = /** @type {ATProtoPublishElement | null} */ (
  /** @type {unknown} */ (document.querySelector(PUBLISH_NAME))
);

if (!existing) {
  const created = new ATProtoPublish();
  created.setAttribute(MARKER, "");
  created.setAttribute("group", foundation.GROUP);
  created.setAttribute("output-selector", outputOrchestrator.selector);
  document.body.append(created);
  existing = created;
}

const account = existing;

await account.whenRestored();

const $working = signal(false);
const $error = signal(/** @type {string | null} */ (null));

////////////////////////////////////////////
// HANDLE TYPEAHEAD
////////////////////////////////////////////

const TYPEAHEAD_URL =
  "https://public.api.bsky.app/xrpc/app.bsky.actor.searchActorsTypeahead";
const TYPEAHEAD_DEBOUNCE_MS = 200;
const TYPEAHEAD_MIN_LENGTH = 2;

const $open = signal(false);
const $active = signal(-1);
const $suggestions = signal(/** @type {HandleSuggestion[]} */ ([]));

/** @type {ReturnType<typeof setTimeout> | null} */
let searchTimer = null;

/** @type {AbortController | null} */
let searchController = null;

/** @param {string} query */
function scheduleSearch(query) {
  if (searchTimer !== null) clearTimeout(searchTimer);

  // Drop any in-flight request so its results can't land for an older query.
  searchController?.abort();
  searchController = null;

  const q = query.trim().replace(/^@/, "");

  if (q.length < TYPEAHEAD_MIN_LENGTH) {
    $suggestions.value = [];
    $open.value = false;
    $active.value = -1;
    return;
  }

  searchTimer = setTimeout(() => void search(q), TYPEAHEAD_DEBOUNCE_MS);
}

/** @param {string} query */
async function search(query) {
  searchController?.abort();

  const controller = new AbortController();
  searchController = controller;

  try {
    const url = new URL(TYPEAHEAD_URL);
    url.searchParams.set("q", query);
    url.searchParams.set("limit", "8");

    const response = await fetch(url, { signal: controller.signal });
    if (!response.ok) throw new Error(`HTTP ${response.status}`);

    /** @type {{ actors?: Array<{ did: string, handle: string, displayName?: string, avatar?: string }> }} */
    const data = await response.json();

    // A newer query superseded this one.
    if (searchController !== controller) return;

    const suggestions = (data.actors ?? [])
      .filter((actor) => typeof actor?.handle === "string")
      .map((actor) => ({
        did: actor.did,
        handle: actor.handle,
        displayName: actor.displayName,
        avatar: actor.avatar,
      }));

    $suggestions.value = suggestions;
    $open.value = suggestions.length > 0;
    $active.value = -1;
  } catch (err) {
    if (err instanceof Error && err.name === "AbortError") return;

    // Search is a convenience; a failure must not block typing a handle.
    $suggestions.value = [];
    $open.value = false;
    $active.value = -1;
  } finally {
    if (searchController === controller) searchController = null;
  }
}

/** @param {HandleSuggestion} suggestion */
function selectSuggestion(suggestion) {
  const input = /** @type {HTMLInputElement | null} */ (
    document.querySelector("#atmosphere-handle-input")
  );

  if (input) {
    input.value = suggestion.handle;
    input.focus();
  }

  $suggestions.value = [];
  $open.value = false;
  $active.value = -1;
}

function closeSuggestions() {
  if (searchTimer !== null) {
    clearTimeout(searchTimer);
    searchTimer = null;
  }

  searchController?.abort();
  searchController = null;

  $open.value = false;
  $active.value = -1;
}

////////////////////////////////////////////
// UI
////////////////////////////////////////////

const main = document.querySelector("main");
if (!main) throw new Error("No <main> element");

effect(() => {
  const did = account.did();
  const handle = account.handle();
  const syncing = account.syncing();
  const lastSyncedAt = account.lastSyncedAt();
  const working = $working.value;
  const error = $error.value ?? account.error();

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
        <h1>Atmosphere</h1>
        <p>
          Connect your AT Protocol account. Atmosphere-powered features in
          Diffuse use this account; connecting it does not change where your
          tracks, settings or other user data are stored.
        </p>
      </div>

      <div class="facet__right">
        <section>
          <h2>Account</h2>

          ${did
            ? html`
              <p>Connected as <strong>${handle ?? did}</strong>.</p>

              ${error
                ? html`<div class="callout callout--danger">${error}</div>`
                : nothing}

              <div class="button-row">
                <button ?disabled="${working}" @click="${disconnect}">
                  <i class="ph-bold ph-plugs-connected"></i>
                  Disconnect
                </button>
              </div>
            `
            : html`
              ${error
                ? html`<div class="callout callout--danger">${error}</div>`
                : nothing}

              <div id="atmosphere-handle-field"></div>
            `}
        </section>

        <hr>

        <section>
          <h2>Publishing</h2>
          <p class="caption">
            Publish your facets, playlists and collaborations while the
            “Publish on the Atmosphere” feature is enabled.
          </p>

          ${did
            ? html`
              ${lastSyncedAt
                ? html`
                  <p class="caption">
                    Last published ${new Date(lastSyncedAt).toLocaleString()}.
                  </p>
                `
                : nothing}

              <div class="button-row">
                <button
                  ?disabled="${syncing || working}"
                  @click="${publishNow}"
                >
                  <i
                    class="ph-bold ${syncing
                      ? "ph-spinner animate-spin"
                      : "ph-cloud-arrow-up"}"
                  ></i>
                  ${syncing ? "Publishing …" : "Publish now"}
                </button>
              </div>
            `
            : html`
              <p class="caption">
                Available once an account is connected.
              </p>
            `}
        </section>

        <!-- Add further Atmosphere-powered features (e.g. spaces) here. -->
      </div>
    `,
    main,
  );
});

/**
 * The handle field is rendered separately so typing does not re-render the
 * whole page (which would also fight with the typeahead state).
 */
effect(() => {
  // Re-run when the account state changes so the field comes back after a
  // disconnect (and stays gone while connected).
  if (account.did()) return;

  const container = /** @type {HTMLElement | null} */ (
    document.querySelector("#atmosphere-handle-field")
  );
  if (!container) return;

  const working = $working.value;
  const open = $open.value;
  const active = $active.value;
  const suggestions = $suggestions.value;

  litRender(
    html`
      <form @submit="${connect}">
        <label class="handle-field">
          Handle
          <span class="handle-field__input">
            <input
              id="atmosphere-handle-input"
              placeholder="you.bsky.social"
              autocomplete="username"
              spellcheck="false"
              role="combobox"
              aria-autocomplete="list"
              aria-expanded="${open}"
              aria-controls="atmosphere-handle-suggestions"
              aria-activedescendant="${active >= 0
                ? `atmosphere-handle-option-${active}`
                : nothing}"
              @input="${onInput}"
              @keydown="${onKeyDown}"
              @focus="${onFocus}"
              @blur="${onBlur}"
              required
            />
            ${open
              ? html`
                <ul
                  id="atmosphere-handle-suggestions"
                  class="handle-field__suggestions"
                  role="listbox"
                >
                  ${suggestions.map((suggestion, index) =>
                    html`
                      <li
                        id="atmosphere-handle-option-${index}"
                        class="handle-field__suggestion ${index === active
                          ? "is-active"
                          : ""}"
                        role="option"
                        aria-selected="${index === active}"
                        @mousedown="${(/** @type {MouseEvent} */ e) => {
                          e.preventDefault();
                          selectSuggestion(suggestion);
                        }}"
                      >
                        <span class="handle-field__avatar">
                          <i class="ph-bold ph-user"></i>
                          ${suggestion.avatar
                            ? html`
                              <img
                                src="${suggestion.avatar}"
                                alt=""
                                loading="lazy"
                                decoding="async"
                              />
                            `
                            : nothing}
                        </span>
                        <span class="handle-field__info">
                          <span class="handle-field__handle">
                            @${suggestion.handle}
                          </span>
                          ${suggestion.displayName
                            ? html`
                              <span class="handle-field__name">
                                ${suggestion.displayName}
                              </span>
                            `
                            : nothing}
                        </span>
                      </li>
                    `
                  )}
                </ul>
              `
              : nothing}
          </span>
        </label>

        <p class="button-row">
          <button ?disabled="${working}">
            <i
              class="ph-bold ${working ? "ph-spinner animate-spin" : "ph-at"}"
            ></i>
            ${working ? "Connecting …" : "Connect"}
          </button>
        </p>
      </form>
    `,
    container,
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/** @param {SubmitEvent} e */
async function connect(e) {
  e.preventDefault();
  $error.value = null;

  const input = /** @type {HTMLInputElement | null} */ (
    /** @type {HTMLFormElement} */ (e.currentTarget).querySelector("input")
  );
  const handle = input?.value?.trim().replace(/^@/, "");
  if (!handle) return;

  $working.value = true;

  try {
    await account.login(handle);
  } catch (err) {
    $error.value = err instanceof Error ? err.message : "Could not connect";
  } finally {
    $working.value = false;
  }
}

/** @param {Event} e */
function onInput(e) {
  scheduleSearch(/** @type {HTMLInputElement} */ (e.currentTarget).value);
}

function onFocus() {
  if ($suggestions.value.length > 0) $open.value = true;
}

function onBlur() {
  // Let a suggestion's mousedown run before the list disappears.
  setTimeout(() => {
    $open.value = false;
    $active.value = -1;
  }, 100);
}

/** @param {KeyboardEvent} e */
function onKeyDown(e) {
  const suggestions = $suggestions.value;

  if (e.key === "ArrowDown" || e.key === "ArrowUp") {
    if (!suggestions.length) return;

    e.preventDefault();
    $open.value = true;

    const delta = e.key === "ArrowDown" ? 1 : -1;
    $active.value = ($active.value + delta + suggestions.length) %
      suggestions.length;
    return;
  }

  if (e.key === "Enter" && $open.value && $active.value >= 0) {
    e.preventDefault();
    selectSuggestion(suggestions[$active.value]);
    return;
  }

  if (e.key === "Escape") {
    if (!$open.value) return;
    e.preventDefault();
    closeSuggestions();
  }
}

async function publishNow() {
  $error.value = null;
  $working.value = true;

  try {
    await account.publish();
  } finally {
    $working.value = false;
  }
}

async function disconnect() {
  $error.value = null;
  $working.value = true;

  try {
    await account.logout();
  } catch (err) {
    $error.value = err instanceof Error ? err.message : "Could not disconnect";
  } finally {
    $working.value = false;
  }
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
