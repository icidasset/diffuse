import { html, nothing, render as litRender } from "lit-html";

import foundation from "~/common/foundation.js";
import { effect, signal } from "~/common/signal.js";

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

// Set doc title
foundation.setup({ title: "Rocksky | Scrobble | Diffuse" });

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

////////////////////////////////////////////
// REACTIVE UI
////////////////////////////////////////////

const $signingIn = signal(false);

const main = document.querySelector("main");

effect(() => {
  const isAuthenticated = rocksky.isAuthenticated();
  const isAuthenticating = rocksky.isAuthenticating();
  const handle = rocksky.handle();
  const signingIn = $signingIn.value;

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
        <h1>Rocksky</h1>
        <p>Scrobble tracks to Rocksky using your AT Protocol identity.</p>
      </div>

      <div class="facet__right">
        ${isAuthenticated
          ? html`
              <div>
                ${handle
                  ? html`<p>Connected as <strong>${handle}</strong>.</p>`
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
                  <label
                    >Handle
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

async function handleSignIn(e) {
  e.preventDefault();
  const handle = e.target.querySelector("input")?.value?.trim();
  if (!handle) return;
  $signingIn.value = true;
  try {
    await rocksky.signIn(handle);
  } finally {
    $signingIn.value = false;
  }
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
