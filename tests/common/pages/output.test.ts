import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/pages/output", () => {
  it("resolves when the selected output never appears", async () => {
    const result = await testWeb(async () => {
      const key = "diffuse/configurator/output/selected/id";
      localStorage.setItem(key, "do-output__dc-output__missing");

      const foundation = (await import("~/common/foundation.js")).default;
      const { output } = await import("~/common/pages/output.js");

      await foundation.orchestrator.output();

      const resolved = await Promise.race([
        output().then(() => true),
        new Promise((resolve) => setTimeout(() => resolve(false), 5_000)),
      ]);

      localStorage.removeItem(key);
      return resolved;
    });

    expect(result).toBe(true);
  });
});
