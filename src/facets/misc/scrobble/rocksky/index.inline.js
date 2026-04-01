import { login } from "~/components/output/raw/atproto/oauth.js";
import { finalizeAuthorization } from "@atcute/oauth-browser-client";

import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

// Set doc title
foundation.setup({ title: "Rocksky | Scrobble | Diffuse" });

const ATPROTO_DID_KEY = "diffuse/output/raw/atproto/did";

// Handle AT Protocol OAuth callback if returning from it.
// The /oauth/callback page passes the #code fragment back to this page's URL.
// We only finalize if the code is actually present — never attempt session
// restoration, as its error path calls clearStoredSession() which would wipe
// the main app's AT Protocol session from localStorage and IndexedDB.
let freshAtprotoSession = null;
const hashParams = new URLSearchParams(location.hash.slice(1));
if (hashParams.has("code")) {
  try {
    const result = await finalizeAuthorization(hashParams);
    history.replaceState(null, "", location.pathname + location.search);
    localStorage.setItem(ATPROTO_DID_KEY, result.session.info.sub);
    freshAtprotoSession = result.session;
  } catch (err) {
    console.warn("rocksky: failed to finalize AT Protocol auth", err);
  }
}

const configurator = await foundation.configurator.scrobbles();

/** @type {import("~/components/supplement/rocksky/element.js").CLASS | null} */
let rocksky = configurator.querySelector("ds-rocksky-scrobbler");
if (!rocksky) {
  const { default: RockskyScrobbler } = await import(
    "~/components/supplement/rocksky/element.js"
  );

  rocksky = new RockskyScrobbler();
  rocksky.setAttribute("group", foundation.GROUP);
  configurator.append(rocksky);
}

await customElements.whenDefined(rocksky.localName);

// If AT Protocol was just authorized via OAuth, immediately connect to Rocksky
if (freshAtprotoSession && !rocksky.isAuthenticated()) {
  rocksky.signIn().catch(() => {});
}

////////////////////////////////////////////
// ELEMENTS
////////////////////////////////////////////

const stateConnect = /** @type {HTMLElement} */ (
  document.querySelector("#state-connect")
);

const stateConnected = /** @type {HTMLElement} */ (
  document.querySelector("#state-connected")
);

const stateNoAtproto = /** @type {HTMLElement} */ (
  document.querySelector("#state-no-atproto")
);

const stateHasAtproto = /** @type {HTMLElement} */ (
  document.querySelector("#state-has-atproto")
);

const handleParagraph = /** @type {HTMLElement} */ (
  document.querySelector("#handle-paragraph")
);

const handleText = /** @type {HTMLElement} */ (
  document.querySelector("#handle-text")
);

const handleInput = /** @type {HTMLInputElement} */ (
  document.querySelector("#handle-input")
);

const atprotoSignInBtn = /** @type {HTMLElement} */ (
  document.querySelector("#atproto-sign-in-btn")
);

const signInBtn = /** @type {HTMLElement} */ (
  document.querySelector("#sign-in-btn")
);

const signOutBtn = /** @type {HTMLElement} */ (
  document.querySelector("#sign-out-btn")
);

////////////////////////////////////////////
// REACTIVE UI
////////////////////////////////////////////

const $hasAtprotoSession = signal(!!localStorage.getItem(ATPROTO_DID_KEY));

effect(() => {
  const isAuthenticated = rocksky.isAuthenticated();
  const isAuthenticating = rocksky.isAuthenticating();
  const handle = rocksky.handle();
  const hasAtproto = $hasAtprotoSession.value;

  stateConnect.hidden = isAuthenticated;
  stateConnected.hidden = !isAuthenticated;

  stateNoAtproto.hidden = hasAtproto;
  stateHasAtproto.hidden = !hasAtproto;

  handleParagraph.hidden = !handle;
  signOutBtn.hidden = !isAuthenticated;
  if (handle) handleText.textContent = handle;

  // @ts-ignore
  signInBtn.disabled = isAuthenticating;
  // @ts-ignore
  atprotoSignInBtn.disabled = isAuthenticating;
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

atprotoSignInBtn.onclick = async () => {
  const handle = handleInput.value?.trim();
  if (!handle) return;
  await login(handle);
};

signInBtn.onclick = () => rocksky.signIn().catch(() => {});

signOutBtn.onclick = () => rocksky.signOut();

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
