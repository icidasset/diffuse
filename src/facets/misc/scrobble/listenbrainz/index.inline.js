import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

foundation.setup({ title: "ListenBrainz | Scrobble | Diffuse" });

const configurator = await foundation.configurator.scrobbles();

/** @type {import("~/components/supplement/listenbrainz/element.js").CLASS | null} */
let listenBrainz = configurator.querySelector("ds-listenbrainz-scrobbler");
if (!listenBrainz) {
  const { default: ListenBrainzScrobbler } = await import(
    "~/components/supplement/listenbrainz/element.js"
  );

  listenBrainz = new ListenBrainzScrobbler();
  listenBrainz.setAttribute("group", foundation.GROUP);
  configurator.append(listenBrainz);
}

await customElements.whenDefined(listenBrainz.localName);

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

const tokenInput = /** @type {HTMLInputElement} */ (
  document.querySelector("#token-input")
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

effect(() => {
  const isAuthenticated = listenBrainz.isAuthenticated();
  const isAuthenticating = listenBrainz.isAuthenticating();
  const handle = listenBrainz.handle();

  stateConnect.hidden = isAuthenticated;
  stateConnected.hidden = !isAuthenticated;

  handleParagraph.hidden = !handle;
  signOutBtn.hidden = !isAuthenticated;
  if (handle) handleText.textContent = handle;

  // @ts-ignore
  signInBtn.disabled = isAuthenticating;
  tokenInput.disabled = isAuthenticating;
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

signInBtn.onclick = async () => {
  const token = tokenInput.value?.trim();
  if (!token) return;
  try {
    await listenBrainz.signIn(token);
    tokenInput.value = "";
  } catch {
    // element already logs the error
  }
};

signOutBtn.onclick = () => listenBrainz.signOut();

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
