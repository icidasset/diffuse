import { html, render as litRender } from "lit-html";

import { data as facetsData } from "~/facets/index.js";
import foundation from "~/common/foundation.js";

foundation.setup({ title: "Connect | Diffuse" });

/** @param {string} path */
const loaderHref = (path) => `l/?path=${encodeURIComponent(path)}`;

/** @type {Record<string, string>} */
const icons = {
  "facets/connect/atproto-passkey/index.html": "at",
  "facets/connect/atproto-space/index.html": "at",
  "facets/connect/dropbox/index.html": "cloud",
  "facets/connect/https/index.html": "globe",
  "facets/connect/https-json/index.html": "list-bullets",
  "facets/connect/icecast/index.html": "radio",
  "facets/connect/local/index.html": "folder-open",
  "facets/connect/opensubsonic/index.html": "broadcast",
  "facets/connect/s3/index.html": "hard-drives",
  "facets/connect/webdav/index.html": "hard-drive",
};

const recommended = new Set([
  "facets/connect/dropbox/index.html",
  "facets/connect/local/index.html",
  "facets/connect/s3/index.html",
]);

const facets = facetsData
  .filter(
    (f) =>
      f.url.startsWith("facets/connect/") &&
      f.url !== "facets/connect/index.html" &&
      !f.incomplete,
  )
  .map((f) => ({
    name: f.title.replace(/^Connect \/ /, ""),
    description: f.desc,
    icon: icons[f.url] ?? "plug",
    href: loaderHref(f.url),
    isRecommended: recommended.has(f.url),
  }))
  .sort((a, b) => (b.isRecommended ? 1 : 0) - (a.isRecommended ? 1 : 0))
;

const main = document.querySelector("main");
if (!main) throw new Error("No <main> element");

litRender(
  html`
    <div class="connect-index__left">
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
      <h1>Connect</h1>
      <p>
        These are some of the options available to add as an audio source, or to use
        as user-data storage. Some offer both.
      </p>
    </div>
    <div class="connect-index__right">
      <ul class="connect-list">
        ${facets.map(({ name, description, icon, href, isRecommended }) =>
          html`
            <li>
              <a class="connect-item" href="${href}">
                <i class="ph-fill ph-${icon} connect-item__icon"></i>
                <div class="connect-item__info">
                  <span class="connect-item__name">
                    ${name}
                    ${isRecommended ? html`<span class="connect-item__badge">Recommended</span>` : ""}
                  </span>
                  <span class="connect-item__detail">${description}</span>
                </div>
              </a>
            </li>
          `
        )}
      </ul>
    </div>
  `,
  main,
);

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

foundation.ready();
