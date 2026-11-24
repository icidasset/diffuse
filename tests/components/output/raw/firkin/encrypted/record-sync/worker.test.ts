// @ts-nocheck -- the browser evaluate intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { launch } from "@astral/astral";

const PAGE_URL = "http://localhost:3000/testing/index.html";
const WORKER_URL =
  "http://localhost:3000/components/output/raw/firkin/encrypted/record-sync/worker.js";

describe("components/output/raw/firkin/encrypted/record-sync worker", () => {
  it("loads the element in a SharedWorker and exposes status", async () => {
    await using browser = await launch();
    const page = await browser.newPage(PAGE_URL);

    const result = await page.evaluate(async (urls) => {
      const { RpcChannel } = await import("~/common/worker/rpc-channel.js");

      /** @type {any[]} */
      const events = [];
      /** @type {string[]} */
      const errors = [];

      const worker = new SharedWorker(urls.worker, {
        name: "record-sync-worker-transport-test",
        type: "module",
      });

      worker.onerror = (event) => {
        errors.push(String(event.message ?? event));
      };

      worker.port.start();
      worker.port.addEventListener("message", (event) => {
        if (event.data?.ns === "record-sync") events.push(event.data);
      });

      const channel = new RpcChannel(worker.port);
      await channel.callMethod("open", [{
        namespace: "worker-test",
        keep: 64,
      }]);

      const deadline = Date.now() + 5_000;
      while (
        Date.now() < deadline &&
        !events.some((event) => event.type === "status")
      ) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }

      // Closing drops the port from the broadcast set; re-opening must add it
      // back and send a fresh snapshot, or a re-mounted element would never
      // see another update.
      await channel.callMethod("close", []);
      await new Promise((resolve) => setTimeout(resolve, 100));
      events.length = 0;

      await channel.callMethod("open", [{
        namespace: "worker-test",
        keep: 64,
      }]);

      const reopened = Date.now() + 5_000;
      while (
        Date.now() < reopened &&
        !events.some((event) => event.type === "status")
      ) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }

      const reopenedStatus = events.find((event) => event.type === "status");

      channel.destroy();

      return { reopenedStatus, errors };
    }, { args: [{ worker: WORKER_URL }] });

    expect(result.errors).toEqual([]);
    expect(result.reopenedStatus?.ready).toBe(false);
  });

  it("selects worker mode for a grouped element", async () => {
    await using browser = await launch();
    const page = await browser.newPage(PAGE_URL);

    const result = await page.evaluate(async () => {
      await import(
        "~/components/output/raw/firkin/encrypted/record-sync/element.js"
      );
      await customElements.whenDefined("dob-record-sync");

      /** @type {string[]} */
      const errors = [];
      const ogError = console.error.bind(console);
      console.error = (...args) => {
        errors.push(args.map(String).join(" "));
        ogError(...args);
      };

      const element = document.createElement("dob-record-sync");
      element.id = "worker-mode-test";
      element.setAttribute("group", "worker-mode-test");
      element.setAttribute("shared", "");
      element.setAttribute("namespace", "worker-mode-test");
      element.setAttribute("keep", "64");
      document.body.append(element);

      await new Promise((resolve) => setTimeout(resolve, 1_500));

      const mode = element.mode;
      const ready = element.ready();

      element.remove();

      return { mode, ready, errors };
    });

    expect(result.mode).toBe("shared-worker");
    expect(result.ready).toBe(false);
    expect(result.errors).toEqual([]);
  });
});
