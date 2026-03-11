import "@awesome.me/webawesome/dist/components/card/card.js";
import "@awesome.me/webawesome/dist/components/button/button.js";
import "@awesome.me/webawesome/dist/components/drawer/drawer.js";
import "@awesome.me/webawesome/dist/components/input/input.js";
import "@awesome.me/webawesome/dist/components/icon/icon.js";

import "~/common/webawesome/detect-dark.js";

import LastFmSupplement from "~/components/supplement/last.fm/element.js";
import { effect } from "~/common/signal.js";

/**
 * @import { default as WaDrawer } from "@awesome.me/webawesome/dist/components/drawer/drawer.js"
 * @import { default as WaInput } from "@awesome.me/webawesome/dist/components/input/input.js"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const CREDS_KEY = "diffuse/supplement/last.fm/credentials";

/** @returns {{ apiKey: string, apiSecret: string } | null} */
function loadCredentials() {
  try {
    return JSON.parse(localStorage.getItem(CREDS_KEY) ?? "null");
  } catch {
    return null;
  }
}

// Find existing or create new ds-lastfm element
let lastFm = /** @type {LastFmSupplement | null} */ (
  document.body.querySelector("ds-lastfm")
);

if (!lastFm) {
  lastFm = new LastFmSupplement();
  const creds = loadCredentials();
  if (creds) {
    lastFm.setAttribute("api-key", creds.apiKey);
    lastFm.setAttribute("api-secret", creds.apiSecret);
  }
  document.body.append(lastFm);
}

await customElements.whenDefined(lastFm.localName);

////////////////////////////////////////////
// ELEMENTS
////////////////////////////////////////////

const stateConnect = /** @type {HTMLElement} */ (
  document.querySelector("#state-connect")
);
const stateConnected = /** @type {HTMLElement} */ (
  document.querySelector("#state-connected")
);
const handleParagraph = /** @type {HTMLElement} */ (
  document.querySelector("#handle-paragraph")
);
const handleText = /** @type {HTMLElement} */ (
  document.querySelector("#handle-text")
);

const settingsBtn = /** @type {HTMLElement} */ (
  document.querySelector("#settings-btn")
);

const signInBtn = /** @type {HTMLElement} */ (
  document.querySelector("#sign-in-btn")
);

const signOutBtn = /** @type {HTMLElement} */ (
  document.querySelector("#sign-out-btn")
);

const credentialsDrawer = /** @type {WaDrawer} */ (
  document.querySelector("#credentials-drawer")
);

const apiKeyInput = /** @type {WaInput} */ (
  document.querySelector("#api-key-input")
);

const apiSecretInput = /** @type {WaInput} */ (
  document.querySelector("#api-secret-input")
);

const saveCredsBtn = /** @type {HTMLElement} */ (
  document.querySelector("#save-creds-btn")
);

const resetCredsBtn = /** @type {HTMLElement} */ (
  document.querySelector("#reset-creds-btn")
);

// Pre-fill drawer inputs with stored credentials
const existingCreds = loadCredentials();
if (existingCreds) {
  apiKeyInput.value = existingCreds.apiKey;
  apiSecretInput.value = existingCreds.apiSecret;
}

////////////////////////////////////////////
// REACTIVE UI
////////////////////////////////////////////

effect(() => {
  const isAuthenticated = lastFm.isAuthenticated();
  const handle = lastFm.handle();

  stateConnect.hidden = isAuthenticated;
  stateConnected.hidden = !isAuthenticated;

  handleParagraph.hidden = !handle;
  signOutBtn.hidden = !isAuthenticated;
  if (handle) handleText.textContent = handle;
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

settingsBtn?.addEventListener("click", (e) => {
  e.stopPropagation();
  credentialsDrawer.open = true;
});

signInBtn.onclick = () => lastFm.signIn();

signOutBtn.onclick = () => lastFm.signOut();

saveCredsBtn.onclick = () => {
  const apiKey = apiKeyInput.value?.trim();
  const apiSecret = apiSecretInput.value?.trim();
  if (!apiKey || !apiSecret) return;

  localStorage.setItem(CREDS_KEY, JSON.stringify({ apiKey, apiSecret }));
  lastFm.setAttribute("api-key", apiKey);
  lastFm.setAttribute("api-secret", apiSecret);
};

resetCredsBtn.onclick = () => {
  localStorage.removeItem(CREDS_KEY);
  lastFm.removeAttribute("api-key");
  lastFm.removeAttribute("api-secret");
  apiKeyInput.value = "";
  apiSecretInput.value = "";
};
