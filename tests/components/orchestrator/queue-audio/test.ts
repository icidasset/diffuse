import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/orchestrator/queue-audio", () => {
  it("only treats URLs inside the refresh window as expiring", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/orchestrator/queue-audio/element.js"
      );

      const now = 1_000_000_000_000; // ms
      const seconds = now / 1000;
      const skew = mod.URL_REFRESH_SKEW_SECONDS;

      return {
        skew,
        fresh: mod.isExpiring(seconds + 6 * 60 * 60, now),
        windowStart: mod.isExpiring(seconds + skew, now),
        insideWindow: mod.isExpiring(seconds + skew - 1, now),
        expired: mod.isExpiring(seconds - 1, now),
        infinite: mod.isExpiring(Infinity, now),
        missing: mod.isExpiring(undefined, now),
      };
    });

    expect(result.skew).toBe(300);
    expect(result.fresh).toBe(false);
    expect(result.windowStart).toBe(true);
    expect(result.insideWindow).toBe(true);
    expect(result.expired).toBe(true);
    expect(result.infinite).toBe(false);
    expect(result.missing).toBe(false);
  });
});
