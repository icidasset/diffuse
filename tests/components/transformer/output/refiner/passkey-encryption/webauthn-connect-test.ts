import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { launch } from "@astral/astral";

/**
 * Regression test for the connect facet's passkey flows.
 *
 * Previously, clicking "Set up new key" / "Use existing key" on the AT
 * Protocol (Public) connect page left the UI stuck on "Setting up …" /
 * "Authenticating …" forever: the passkey transformer sits on top of the
 * atproto-sync transformer, but that custom element was only defined once the
 * output had been *activated* (which hasn't happened on the connect page), so
 * `base()` collections never reached "loaded" and `#rekey` never settled.
 *
 * Drives the real (built) connect page with Chrome's virtual WebAuthn
 * authenticator (with PRF support) via CDP.
 */

const URL =
  "http://localhost:3000/l/?path=facets/connect/atproto-passkey/index.tile";

describe("facets/connect/atproto-passkey", () => {
  it(
    "set up new key and use existing key both settle (buttons re-enable)",
    // @ts-ignore — long timeout
    { timeout: 120_000 },
    async () => {
      await using browser = await launch({ args: ["--no-sandbox"] });
      await using page = await browser.newPage(URL, { waitUntil: "load" });

      const errors: string[] = [];
      page.addEventListener("console", (e) => {
        const { type, text } = /** @type {any} */ (e).detail ??
          /** @type {any} */ (e);
        if (type === "error") errors.push(text);
      });
      page.addEventListener("pageerror", (e) => {
        errors.push(String(e));
      });

      const c = page.unsafelyGetCelestialBindings();
      await c.WebAuthn.enable({ enableUI: false });
      await c.WebAuthn.addVirtualAuthenticator({
        options: {
          protocol: "ctap2",
          ctap2Version: "ctap2_1",
          transport: "internal",
          hasResidentKey: true,
          hasUserVerification: true,
          hasPrf: true,
          automaticPresenceSimulation: true,
          isUserVerified: true,
        },
      });

      const getButtonStates = async () => {
        return await page.evaluate(() => {
          const btns = Array.from(
            document.querySelectorAll("main .facet__right button"),
          );
          return btns.map((b) => b.textContent?.trim() ?? "");
        });
      };

      const getVisibleError = async () => {
        return await page.evaluate(() => {
          const el = [...document.querySelectorAll(".callout--danger")]
            .filter((el) => !el.hasAttribute("hidden"))
            .find((el) => el.textContent?.trim());
          return el?.textContent?.trim() ?? null;
        });
      };

      /** Wait until the buttons reflect `expected` (or an error shows). */
      const waitForSettlement = async (expected: string) => {
        const deadline = Date.now() + 30_000;
        while (Date.now() < deadline) {
          const states = await getButtonStates();
          const err = await getVisibleError();
          if (err) throw new Error(`Unexpected error callout: ${err}`);
          if (states.includes(expected)) return states;
          await new Promise((r) => setTimeout(r, 250));
        }
        const states = await getButtonStates();
        throw new Error(
          `Never settled on "${expected}". states: ${JSON.stringify(states)} errors: ${JSON.stringify(errors)}`,
        );
      };

      // Wait until the passkey section renders
      {
        const deadline = Date.now() + 30_000;
        let found = false;
        while (Date.now() < deadline) {
          found = await page.evaluate(() => {
            return [...document.querySelectorAll("main .facet__right *")]
              .some((el) => el.textContent?.includes("Passkey encryption"));
          });
          if (found) break;
          await new Promise((r) => setTimeout(r, 250));
        }
        expect(found, "passkey section should render").toBe(true);
      }

      const facetRight = await page.$("main .facet__right");
      const facetButtons = facetRight ? await facetRight.$$("button") : [];
      const findButton = async (
        handles: import("@astral/astral").ElementHandle[],
        label: string,
      ) => {
        for (const h of handles) {
          const text = await h.evaluate(
            (el: HTMLElement) => el.textContent?.trim() ?? "",
          );
          if (text === label) return h;
        }
        return null;
      };

      // Phase 1: set up a new key (creates a credential on the authenticator).
      const setupBtn = await findButton(facetButtons, "Set up new key");
      expect(setupBtn, "Set up new key button").not.toBeNull();
      await setupBtn!.click();

      // Should settle into the configured state instead of staying stuck.
      await waitForSettlement("Use other existing key");
      const configured = await page.evaluate(() => {
        const el = document.querySelector(
          'dtor-passkey-encryption[namespace="atproto-passkey"]',
        ) as { passkeyActive(): boolean } | null;
        return el?.passkeyActive?.() ?? false;
      });
      expect(configured, "passkey should be active after setup").toBe(true);

      // Phase 2: adopt the existing key ("Use other existing key").
      await page.evaluate(() => {
        const btn = [...document.querySelectorAll("main .facet__right button")]
          .find((b) => b.getAttribute("aria-label") === "Passkey options");
        (btn as HTMLElement | null)?.click();
      });
      await new Promise((r) => setTimeout(r, 500));

      const menu = await page.$("#atproto-passkey-menu");
      const menuButtons = menu ? await menu.$$("button") : [];
      const adoptBtn = await findButton(
        menuButtons,
        "Use other existing key",
      );
      expect(adoptBtn, "Use other existing key button").not.toBeNull();
      await adoptBtn!.click();

      // Should settle back into the configured state, not hang.
      await waitForSettlement("Use other existing key");
    },
  );
});