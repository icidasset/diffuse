import { html, render as litRender } from "lit-html";

import "@awesome.me/webawesome/dist/components/card/card.js";
import "@awesome.me/webawesome/dist/components/divider/divider.js";
import "@awesome.me/webawesome/dist/components/icon/icon.js";

import "~/common/webawesome/detect-dark.js";
import "~/common/webawesome/phosphor/fill.js";

import { data as facetsData } from "~/facets/index.js";

document.title = "Connect | Diffuse";

/** @param {string} path */
const loaderHref = (path) => `../../l/?path=${encodeURIComponent(path)}`;

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
    <wa-card>
      <div slot="header" class="card-header">
        <strong>Connect</strong>
      </div>
      <div class="card-body">
        <p>
          These are some of the options available to add as an audio source, or to
          use as user-data storage. Some offer both.
        </p>
        <wa-divider></wa-divider>
        <ul class="connect-list">
          ${facets.map(({ name, description, icon, href }) =>
            html`
              <li>
                <a class="connect-item" href="${href}">
                  <wa-icon
                    library="phosphor/fill"
                    name="${icon}"
                    class="wa-body-l"
                    style="flex-shrink: 0;"
                  ></wa-icon>
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
    </wa-card>
  `,
  main,
);
