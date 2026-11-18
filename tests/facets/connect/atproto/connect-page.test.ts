// @ts-nocheck -- page driving is untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { launch } from "@astral/astral";

const CONNECT_URL =
  "http://localhost:3000/l/?path=facets%2Fconnect%2Fatproto%2Findex.tile";

/**
 * The connect page waits for the account's initial session restore, so
 * `networkidle` may never settle. Wait for the load event instead.
 */
const NAVIGATE = { waitUntil: "load" };

/**
 * Wait until the connect page has rendered, then report what it shows.
 *
 * Standalone (no captures) so it can be handed to `page.evaluate`.
 */
async function atmospherePageState() {
  const deadline = Date.now() + 15_000;

  const ready = () =>
    Boolean(document.querySelector("main form input")) &&
    !document.querySelector("#diffuse-loader");

  while (Date.now() < deadline && !ready()) {
    await new Promise((resolve) => setTimeout(resolve, 100));
  }

  const text = document.querySelector("main")?.textContent ?? "";

  return {
    rendered: ready(),
    loaderPresent: Boolean(document.querySelector("#diffuse-loader")),
    hasLoaded:
      document.querySelector("#container")?.classList.contains("has-loaded") ??
        false,
    mentionsAccount: text.includes("Atmosphere"),
    mentionsPublishing: text.includes("Publishing"),
    hasHandleInput: Boolean(document.querySelector("main form input")),
    hasConnectButton:
      (document.querySelector("main form button")?.textContent ?? "").includes(
        "Connect",
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
      return await page.evaluate(atmospherePageState);
    } catch (err) {
      lastError = err;
      await new Promise((resolve) => setTimeout(resolve, 250));
    }
  }

  throw lastError;
}

describe("connect/atproto", () => {
  it("renders the account and publishing sections without a session", async () => {
    await using browser = await launch();
    const page = await browser.newPage(CONNECT_URL, NAVIGATE);

    const result = await readConnectPage(page);

    expect(result.rendered).toBe(true);
    expect(result.loaderPresent).toBe(false);
    expect(result.hasLoaded).toBe(true);
    expect(result.mentionsAccount).toBe(true);
    expect(result.mentionsPublishing).toBe(true);
    expect(result.hasHandleInput).toBe(true);
    expect(result.hasConnectButton).toBe(true);
  });
});
