import { html, render as litRender } from "lit-html";

import { data as facetsData } from "~/facets/index.js";
import foundation from "~/common/foundation.js";

foundation.setup({ title: "Connect | Diffuse" });

/** @param {string} path */
const loaderHref = (path) => `l/?path=${encodeURIComponent(path)}`;

/** @type {Record<string, string>} */
const icons = {
  "facets/connect/atproto/index.html": "at",
  "facets/connect/https/index.html": "globe",
  "facets/connect/icecast/index.html": "radio",
  "facets/connect/local/index.html": "folder-open",
  "facets/connect/opensubsonic/index.html": "broadcast",
  "facets/connect/s3/index.html": "hard-drives",
};

const facets = facetsData
  .filter(
    (f) =>
      f.url.startsWith("facets/connect/") &&
      f.url !== "facets/connect/index.html",
  )
  .map((f) => ({
    name: f.title.replace(/^Connect \/ /, ""),
    description: f.desc,
    icon: icons[f.url] ?? "plug",
    href: loaderHref(f.url),
  }));

const main = document.querySelector("main");
if (!main) throw new Error("No <main> element");

litRender(
  html`
    <div class="connect-index__left">
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
      <h1>Connect</h1>
      <p>
        These are some of the options available to add as an audio source, or to use
        as user-data storage. Some offer both.
      </p>
    </div>
    <div class="connect-index__right">
      <ul class="connect-list">
        ${facets.map(({ name, description, icon, href }) =>
          html`
            <li>
              <a class="connect-item" href="${href}">
                <i class="ph-fill ph-${icon} connect-item__icon"></i>
                <div class="connect-item__info">
                  <span class="connect-item__name">${name}</span>
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
