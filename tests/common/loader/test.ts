import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/loader", () => {
  describe("renderError", () => {
    it("sets innerHTML on the container", async () => {
      const result = await testWeb(async () => {
        const { renderError } = await import("~/common/loader.js");
        const container = document.createElement("div");
        renderError(container, "Something went wrong");
        return container.innerHTML.length > 0;
      });
      expect(result).toBe(true);
    });

    it("includes the error message in the output", async () => {
      const result = await testWeb(async () => {
        const { renderError } = await import("~/common/loader.js");
        const container = document.createElement("div");
        renderError(container, "Track not found");
        return container.innerHTML;
      });
      expect(result).toContain("Track not found");
    });

    it("does not throw when options.throw is false", async () => {
      const result = await testWeb(async () => {
        const { renderError } = await import("~/common/loader.js");
        const container = document.createElement("div");
        try {
          renderError(container, "oops", { throw: false });
          return "no-throw";
        } catch {
          return "threw";
        }
      });
      expect(result).toBe("no-throw");
    });

    it("throws when options.throw is true", async () => {
      const result = await testWeb(async () => {
        const { renderError } = await import("~/common/loader.js");
        const container = document.createElement("div");
        try {
          renderError(container, "fatal error", { throw: true });
          return "no-throw";
        } catch (e) {
          return (e as Error).message;
        }
      });
      expect(result).toBe("fatal error");
    });

    it("throws the provided context error", async () => {
      const result = await testWeb(async () => {
        const { renderError } = await import("~/common/loader.js");
        const container = document.createElement("div");
        const ctx = new Error("context error");
        try {
          renderError(container, "label", { context: ctx, throw: true });
          return null;
        } catch (e) {
          return (e as Error).message;
        }
      });
      expect(result).toBe("context error");
    });
  });

  describe("ensureHTML", () => {
    it("returns item unchanged when html is already set", async () => {
      const result = await testWeb(async () => {
        const { ensureHTML } = await import("~/common/loader.js");
        const item = { html: "<p>existing</p>", uri: "https://example.com/facet.html" };
        const returned = await ensureHTML(item);
        return { sameRef: returned === item, html: returned.html };
      });
      expect(result.sameRef).toBe(true);
      expect(result.html).toBe("<p>existing</p>");
    });

    it("returns item unchanged when uri is absent", async () => {
      const result = await testWeb(async () => {
        const { ensureHTML } = await import("~/common/loader.js");
        const item = { html: undefined, uri: undefined };
        const returned = await ensureHTML(item);
        return returned.html ?? null;
      });
      expect(result).toBe(null);
    });
  });
});
