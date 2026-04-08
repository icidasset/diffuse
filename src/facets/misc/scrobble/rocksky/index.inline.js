import { html, nothing, render as litRender } from "lit-html";

import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";
import { NAME as ATPROTO_NAME } from "~/components/output/raw/atproto/element.js";

/**
 * @import { ATProtoOutputElement } from "~/components/output/raw/atproto/types.d.ts"
 */

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

// Set doc title
foundation.setup({ title: "Rocksky | Scrobble | Diffuse" });

const [configurator, output] = await Promise.all([
  foundation.configurator.scrobbles(),
  foundation.orchestrator.output(),
]);

/** @type {import("~/components/supplement/rocksky/element.js").CLASS | null} */
let rocksky = configurator.querySelector("ds-rocksky-scrobbler");
if (!rocksky) {
  const { default: RockskyScrobbler } = await import(
    "~/components/supplement/rocksky/element.js"
  );

  rocksky = new RockskyScrobbler();
  rocksky.setAttribute("group", foundation.GROUP);
  rocksky.setAttribute("output-selector", output.selector);
  configurator.append(rocksky);
} else {
  rocksky.setAttribute("output-selector", output.selector);
}

await customElements.whenDefined(rocksky.localName);
const atprotoEl = signal(
  /** @type {ATProtoOutputElement | undefined} */ (undefined),
);

customElements.whenDefined(ATPROTO_NAME).then(async () => {
  const outputOrchestrator = await foundation.orchestrator.output();

  atprotoEl.value = /** @type {ATProtoOutputElement | undefined} */ (
    outputOrchestrator.root().querySelector(ATPROTO_NAME)
  );
});

////////////////////////////////////////////
// REACTIVE UI
////////////////////////////////////////////

const $signingIn = signal(false);

const main = document.querySelector("main");
if (!main) throw new Error("main element not found");

effect(() => {
  const isAuthenticated = rocksky.isAuthenticated();
  const isAuthenticating = rocksky.isAuthenticating();
  const handle = rocksky.handle();
  const signingIn = $signingIn.value;
  const atprotoDid = atprotoEl.value?.did();
  const atprotoHandle = atprotoEl.value?.handle();

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
              >
              </use>
            </svg>
          </a>
        </div>
        <h1>Rocksky</h1>
        <p>Scrobble tracks to Rocksky using your AT Protocol identity.</p>
      </div>

      <div class="facet__right">
        ${isAuthenticated
          ? html`
            <div>
              ${handle
                ? html`
                  <p>Connected as <strong>${handle}</strong>.</p>
                `
                : nothing}
              <div class="button-row">
                <button @click="${() => rocksky.signOut()}">
                  <i class="ph-bold ph-plugs-connected"></i>
                  Disconnect
                </button>
              </div>
            </div>
          `
          : html`
            <div>
              <p>Sign in with your AT Protocol identity to connect Rocksky.</p>
              <form @submit="${handleSignIn}">
                <label>Handle
                  <input placeholder="you.bsky.social" />
                </label>
                <p class="button-row">
                  <button ?disabled="${isAuthenticating || signingIn}">
                    <i
                      class="ph-bold ${isAuthenticating || signingIn
                        ? "ph-spinner animate-spin"
                        : "ph-at"}"
                    ></i>
                    Connect with AT Protocol
                  </button>
                </p>
              </form>
              ${atprotoDid
                ? html`
                  <p class="button-row">
                    <button
                      ?disabled="${isAuthenticating || signingIn}"
                      @click="${() => handleSignInWithAtproto(atprotoDid)}"
                      class="button--bg-accent"
                    >
                      <i
                        class="ph-bold ${isAuthenticating || signingIn
                          ? "ph-spinner animate-spin"
                          : "ph-at"}"
                      ></i>
                      Sign in as ${atprotoHandle ?? atprotoDid}
                    </button>
                  </p>
                `
                : nothing}
            </div>
          `}
      </div>
    `,
    main,
  );
});

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @param {any} e
 */
async function handleSignIn(e) {
  e.preventDefault();
  const handle = e.target?.querySelector("input")?.value?.trim();
  if (!handle) return;
  $signingIn.value = true;
  try {
    await rocksky?.signIn(handle);
  } finally {
    $signingIn.value = false;
  }
}

/**
 * @param {string} did
 */
async function handleSignInWithAtproto(did) {
  $signingIn.value = true;
  try {
    await rocksky?.signIn(did);
  } finally {
    $signingIn.value = false;
  }
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
