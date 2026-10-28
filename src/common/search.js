////////////////////////////////////////////
// HELPERS
////////////////////////////////////////////

/**
 * Dot paths of every string property in a schema — the fields a search index
 * can match on.
 *
 * @param {Record<string, any>} schema
 * @param {string} [prefix]
 * @returns {string[]}
 *
 * @example Nested and non-string properties
 * ```js
 * import { searchablePaths } from "~/common/search.js";
 *
 * const paths = searchablePaths({
 *   id: "string",
 *   year: "number",
 *   tags: { title: "string", track: { no: "number" } },
 * });
 *
 * if (paths.join() !== "id,tags.title") throw new Error(paths.join());
 * ```
 */
export function searchablePaths(schema, prefix = "") {
  return Object.entries(schema).flatMap(([key, value]) => {
    const path = prefix ? `${prefix}.${key}` : key;

    if (typeof value === "string") {
      return value.startsWith("string") ? [path] : [];
    }

    return searchablePaths(value, path);
  });
}

/**
 * Count each word in the given paths of a document, including duplicates.
 *
 * @param {import("@orama/orama").Tokenizer} tokenizer
 * @param {string[]} paths
 * @param {Record<string, any>} document
 * @returns {Map<string, number>}
 *
 * @example Counts repeated words
 * ```js
 * import * as Orama from "@orama/orama";
 *
 * import { countWords } from "~/common/search.js";
 *
 * const tokenizer = Orama.components.tokenizer.createTokenizer({ allowDuplicates: true });
 * const counts = countWords(tokenizer, ["title"], { title: "Goo Goo Dolls" });
 *
 * if (counts.get("goo") !== 2) throw new Error("repeated words should be counted");
 * ```
 */
export function countWords(tokenizer, paths, document) {
  /** @type {Map<string, number>} */
  const counts = new Map();

  for (const path of paths) {
    let value = /** @type {any} */ (document);
    for (const part of path.split(".")) value = value?.[part];
    if (typeof value !== "string") continue;

    for (const word of tokenizer.tokenize(value)) {
      counts.set(word, (counts.get(word) ?? 0) + 1);
    }
  }

  return counts;
}

/**
 * Whether every keyword can claim its own word. Words match by prefix, like
 * the index does, but each keyword consumes a distinct word — so repeating a
 * term requires it to occur more than once (`goo goo` does not match a lone
 * "Goodbye").
 *
 * @param {Map<string, number>} counts
 * @param {string[]} keywords
 * @returns {boolean}
 *
 * @example Repeated terms need distinct words
 * ```js
 * import { matchesWords } from "~/common/search.js";
 *
 * const one = new Map([["goodbye", 1]]);
 * const two = new Map([["goo", 2]]);
 *
 * if (matchesWords(one, ["goo", "goo"])) throw new Error("one word cannot satisfy two terms");
 * if (!matchesWords(two, ["goo", "goo"])) throw new Error("two words should satisfy two terms");
 * ```
 */
export function matchesWords(counts, keywords) {
  /** @type {string[]} */
  const pool = [];

  for (const [word, count] of counts) {
    for (let i = 0; i < count; i++) pool.push(word);
  }

  // Longer keywords can match fewer words, so satisfy them first.
  for (const keyword of [...keywords].sort((a, b) => b.length - a.length)) {
    const index = pool.findIndex((word) => word.startsWith(keyword));
    if (index === -1) return false;
    pool.splice(index, 1);
  }

  return true;
}
