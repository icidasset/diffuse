// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/firkin/storage", () => {
  it("only advances a record", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { writeIfNewer } = await import("~/common/firkin/storage.js");

      const key = "test/write-if-newer";
      const advance = (next, current) => next.n >= current.n;

      await IDB.del(key);

      await writeIfNewer(key, { n: 2 }, advance);
      await writeIfNewer(key, { n: 1 }, advance);

      const afterOlder = await IDB.get(key);

      await Promise.all([
        writeIfNewer(key, { n: 3 }, advance),
        writeIfNewer(key, { n: 4 }, advance),
      ]);

      const afterConcurrent = await IDB.get(key);

      await IDB.del(key);
      return { afterOlder, afterConcurrent };
    });

    expect(result.afterOlder).toEqual({ n: 2 });
    expect(result.afterConcurrent).toEqual({ n: 4 });
  });
});
