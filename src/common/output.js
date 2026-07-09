import { effect } from "~/common/signal.js";

/**
 * @import {SignalReader} from "~/common/signal.d.ts";
 */

/**
 * Merges two arrays by `id`. Items from `incoming` replace any matching
 * items in `existing`; unmatched existing items are preserved.
 *
 * @template {{ id: string }} T
 * @param {T[]} existing
 * @param {T[]} incoming
 * @returns {T[]}
 *
 * @example Returns incoming items when existing is empty
 * ```js
 * import { mergeById } from "~/common/output.js";
 *
 * const result = mergeById([], [{ id: "a" }, { id: "b" }]);
 * if (result.map(t => t.id).join(",") !== "a,b") throw new Error("unexpected result");
 * ```
 *
 * @example Returns existing items when incoming is empty
 * ```js
 * import { mergeById } from "~/common/output.js";
 *
 * const result = mergeById([{ id: "a" }, { id: "b" }], []);
 * if (result.map(t => t.id).join(",") !== "a,b") throw new Error("unexpected result");
 * ```
 *
 * @example Preserves items not present in incoming
 * ```js
 * import { mergeById } from "~/common/output.js";
 *
 * const result = mergeById([{ id: "a" }, { id: "b" }], [{ id: "c" }]);
 * if (result.map(t => t.id).join(",") !== "a,b,c") throw new Error("unexpected result");
 * ```
 *
 * @example Replaces existing item with incoming version when ids match
 * ```js
 * import { mergeById } from "~/common/output.js";
 *
 * const result = mergeById([{ id: "a", uri: "old://a" }], [{ id: "a", uri: "new://a" }]);
 * if (result.length !== 1) throw new Error("expected length 1");
 * if (result[0].uri !== "new://a") throw new Error("expected new uri");
 * ```
 *
 * @example Preserves other-source items when incoming covers one source
 * ```js
 * import { mergeById } from "~/common/output.js";
 *
 * const existing = [
 *   { id: "s3-1", uri: "s3://bucket/a.flac" },
 *   { id: "s3-2", uri: "s3://bucket/b.flac" },
 *   { id: "wd-1", uri: "webdav://server/c.flac" },
 * ];
 * const incoming = [
 *   { id: "s3-1", uri: "s3://bucket/a.flac" },
 *   { id: "s3-3", uri: "s3://bucket/d.flac" },
 * ];
 * const result = mergeById(existing, incoming);
 * const sorted = result.map(t => t.id).sort().join(",");
 * if (sorted !== "s3-1,s3-2,s3-3,wd-1") throw new Error("unexpected result: " + sorted);
 * ```
 *
 * @example Incoming items appear after preserved existing items
 * ```js
 * import { mergeById } from "~/common/output.js";
 *
 * const result = mergeById([{ id: "x" }], [{ id: "y" }, { id: "z" }]);
 * if (result.map(t => t.id).join(",") !== "x,y,z") throw new Error("unexpected result");
 * ```
 *
 * @example Handles both arrays empty
 * ```ts
 * import { mergeById } from "~/common/output.js";
 * import type { Track } from "~/definitions/types.d.ts";
 *
 * const result = mergeById<Track>([], []);
 * if (result.length !== 0) throw new Error("expected empty result");
 * ```
 */
export function mergeById(existing, incoming) {
  const ids = new Set(incoming.map((t) => t.id));
  const preserved = existing.filter((t) => !ids.has(t.id));
  return [...preserved, ...incoming];
}

/**
 * @template T
 * @param {{ collection: SignalReader<{ state: "loading" } | { state: "loaded"; data: T }> }} output
 * @returns {Promise<T>}
 *
 * @example Resolves immediately when collection is already loaded
 * ```js
 * import { data } from "~/common/output.js";
 * import { signal } from "~/common/signal.js";
 *
 * const col = signal(JSON.parse('{"state":"loaded","data":["a","b"]}'));
 * const result = await data({ collection: col.get });
 * if (result.join(",") !== "a,b") throw new Error("expected ['a', 'b']");
 * ```
 *
 * @example Waits for collection to transition to loaded
 * ```js
 * import { data } from "~/common/output.js";
 * import { signal } from "~/common/signal.js";
 *
 * const col = signal(JSON.parse('{"state":"loading"}'));
 * const promise = data({ collection: col.get });
 *
 * await Promise.resolve();
 * col.set({ state: "loaded", data: [1, 2, 3] });
 *
 * const result = await promise;
 * if (result.join(",") !== "1,2,3") throw new Error("expected [1, 2, 3]");
 * ```
 */
export async function data(output) {
  return await new Promise((resolve) => {
    let resolved = false;

    const stop = effect(() => {
      if (resolved) {
        stop();
        return;
      }

      const col = output.collection();

      if (col.state === "loaded") {
        resolved = true;
        resolve(col.data);
      }
    });
  });
}
