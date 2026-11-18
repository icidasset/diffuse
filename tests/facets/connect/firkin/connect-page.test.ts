// @ts-nocheck -- page driving is untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { launch } from "@astral/astral";

const CONNECT_URL =
  "http://localhost:3000/l/?path=facets%2Fconnect%2Ffirkin%2Findex.tile";

/**
 * The connect page keeps working after load (it waits for its output option),
 * so `networkidle` never settles — wait for the load event instead.
 */
const NAVIGATE = { waitUntil: "load" };

/**
 * Wait until the connect page has rendered, then report what it shows.
 *
 * Standalone (no captures) so it can be handed to `page.evaluate`.
 */
async function connectPageState() {
  const deadline = Date.now() + 15_000;

  const ready = () =>
    Boolean(document.querySelector("#connect-add-input-btn")) &&
    !document.querySelector("#diffuse-loader");

  while (Date.now() < deadline && !ready()) {
    await new Promise((resolve) => setTimeout(resolve, 100));
  }

  return {
    rendered: ready(),
    loaderPresent: Boolean(document.querySelector("#diffuse-loader")),
    hasLoaded:
      document.querySelector("#container")?.classList.contains("has-loaded") ??
        false,
    optionPresent: Boolean(
      document.getElementById("do-output__dc-output__firkin-automerge"),
    ),
    inputButton: Boolean(document.querySelector("#connect-add-input-btn")),
    outputButton: Boolean(document.querySelector("#connect-add-output-btn")),
    mentionsFeature:
      (document.querySelector(".facet__left")?.textContent ?? "").includes(
        "Firkin Output",
      ),
  };
}

/**
 * The loader replaces the document context as it materialises the tile, so a
 * `load`-time evaluate can land in the discarded context.
 *
 * @param {any} page
 */
async function readConnectPage(page) {
  let lastError;

  for (let attempt = 0; attempt < 5; attempt++) {
    try {
      return await page.evaluate(connectPageState);
    } catch (err) {
      lastError = err;
      await new Promise((resolve) => setTimeout(resolve, 250));
    }
  }

  throw lastError;
}

describe("connect/firkin", () => {
  it("renders without the loader when the output feature is off", async () => {
    await using browser = await launch();
    const page = await browser.newPage(CONNECT_URL, NAVIGATE);

    const result = await readConnectPage(page);

    // The output option is never registered, so waiting for it must not keep
    // the page from rendering.
    expect(result.rendered).toBe(true);
    expect(result.loaderPresent).toBe(false);
    expect(result.hasLoaded).toBe(true);
    expect(result.inputButton).toBe(true);

    // No option means no action: the button is hidden and the reason is shown.
    expect(result.optionPresent).toBe(false);
    expect(result.outputButton).toBe(false);
    expect(result.mentionsFeature).toBe(true);
  });

  it("offers the output action once the feature facet is enabled", async () => {
    await using browser = await launch();
    const dashboard = await browser.newPage("http://localhost:3000/dashboard");

    const enabled = await dashboard.evaluate(async () => {
      const deadlineBoot = Date.now() + 20_000;
      let output = null;
      while (Date.now() < deadlineBoot) {
        output = document.querySelector("do-output");
        if (output) break;
        await new Promise((resolve) => setTimeout(resolve, 100));
      }
      if (!output) return "no do-output element";

      const { buildFacets } = await import("~/common/facets/utils.js");
      const { insertPreludes } = await import("~/common/facets/prelude.js");

      const facet = buildFacets([
        "facets/data/firkin-automerge/index.tile",
      ])[0];

      const collection = output.facets.collection();
      const existing = collection.state === "loaded" ? collection.data : [];

      await output.facets.save([
        ...existing.filter((record) => record.id !== facet.id),
        facet,
      ]);

      await insertPreludes([facet]);

      const deadline = Date.now() + 10_000;
      while (
        Date.now() < deadline &&
        !document.getElementById("do-output__dc-output__firkin-automerge")
      ) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }

      return document.getElementById("do-output__dc-output__firkin-automerge")
        ? "enabled"
        : "option missing";
    });

    expect(enabled).toBe("enabled");

    // The facet record is now in the local store, so the connect page's own
    // boot inserts its prelude and the option exists there too.
    const page = await browser.newPage(CONNECT_URL, NAVIGATE);

    const result = await readConnectPage(page);

    expect(result.loaderPresent).toBe(false);
    expect(result.outputButton).toBe(true);
    expect(result.mentionsFeature).toBe(false);
  });
});
