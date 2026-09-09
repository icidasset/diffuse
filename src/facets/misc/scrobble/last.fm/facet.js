import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

// Set doc title
foundation.setup({ title: "Last.fm | Scrobble | Diffuse" });

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const CREDS_KEY = "facets/misc/scrobble/last.fm/credentials";

/** @returns {{ apiKey: string, apiSecret: string } | null} */
function loadCredentials() {
  try {
    return JSON.parse(localStorage.getItem(CREDS_KEY) ?? "null");
  } catch {
    return null;
  }
}

const [configurator, output] = await Promise.all([
  foundation.configurator.scrobbles(),
  foundation.orchestrator.output(),
]);

/** @type {import("~/components/supplement/last.fm/element.js").CLASS | null} */
let lastFm = configurator.querySelector("ds-lastfm-scrobbler");
if (!lastFm) {
  const { default: LastFmScrobbler } = await import(
    "~/components/supplement/last.fm/element.js"
  );

  lastFm = new LastFmScrobbler();
  lastFm.setAttribute("group", foundation.GROUP);
  lastFm.setAttribute("output-selector", output.selector);
  configurator.append(lastFm);
} else {
  lastFm.setAttribute("output-selector", output.selector);
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

const credentialsDialog = /** @type {HTMLDialogElement} */ (
  document.querySelector("#credentials-dialog")
);

const apiKeyInput = /** @type {HTMLInputElement} */ (
  document.querySelector("#api-key-input")
);

const apiSecretInput = /** @type {HTMLInputElement} */ (
  document.querySelector("#api-secret-input")
);

const saveCredsBtn = /** @type {HTMLElement} */ (
  document.querySelector("#save-creds-btn")
);

const resetCredsBtn = /** @type {HTMLElement} */ (
  document.querySelector("#reset-creds-btn")
);

// Pre-fill dialog inputs with stored credentials
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
  const isAuthenticating = lastFm.isAuthenticating();
  const handle = lastFm.handle();

  stateConnect.hidden = isAuthenticated;
  stateConnected.hidden = !isAuthenticated;

  handleParagraph.hidden = !handle;
  signOutBtn.hidden = !isAuthenticated;
  if (handle) handleText.textContent = handle;

  // @ts-ignore
  signInBtn.disabled = isAuthenticating;
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

settingsBtn?.addEventListener("click", (e) => {
  e.stopPropagation();
  credentialsDialog.showModal();
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

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
