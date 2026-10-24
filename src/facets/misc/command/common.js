/**
 * @import {CommandOption} from "kmenu"
 * @import {PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

/**
 * Check whether every query term appears in an option's label, group, or
 * keywords.
 *
 * @param {CommandOption} option
 * @param {string[]} terms
 * @returns {boolean}
 *
 * @example Matches label, keywords, and group case-insensitively
 * ```js
 * import { matchesOption } from "~/facets/misc/command/common.js";
 *
 * const option = {
 *   id: "next",
 *   label: "Next track",
 *   group: "Playback",
 *   keywords: ["skip"],
 * };
 *
 * if (!matchesOption(option, ["next"])) throw new Error("label should match");
 * if (!matchesOption(option, ["skip"])) throw new Error("keyword should match");
 * if (!matchesOption(option, ["playback"])) throw new Error("group should match");
 * if (matchesOption(option, ["shuffle"])) throw new Error("unrelated term should not match");
 * ```
 *
 * @example Requires all terms to match
 * ```js
 * import { matchesOption } from "~/facets/misc/command/common.js";
 *
 * const option = { id: "seek", label: "Forward 10 seconds", keywords: ["skip"] };
 *
 * if (!matchesOption(option, ["forward", "10"])) throw new Error("all terms should match");
 * if (matchesOption(option, ["forward", "60"])) throw new Error("one missing term should fail");
 * ```
 */
export function matchesOption(option, terms) {
  const haystack = [
    option.label,
    option.group ?? "",
    ...(option.keywords ?? []),
  ].join(" ").toLowerCase();

  return terms.every((term) => haystack.includes(term));
}

/**
 * Match an option or any of its descendants. This lets a root command surface
 * when the user searches for something inside its submenu (for example
 * "delete" finding "Remove playlist").
 *
 * @param {CommandOption} option
 * @param {string[]} terms
 * @returns {boolean}
 *
 * @example Matches a descendant even when the parent does not match
 * ```js
 * import { matchesSubtree } from "~/facets/misc/command/common.js";
 *
 * const option = {
 *   id: "playlists",
 *   label: "Remove playlist",
 *   children: [
 *     { id: "confirm", label: "Confirm: remove Rock", keywords: ["delete"] },
 *   ],
 * };
 *
 * if (!matchesSubtree(option, ["delete"])) throw new Error("descendant keyword should match");
 * if (matchesSubtree(option, ["rename"])) throw new Error("unrelated term should not match");
 * ```
 */
export function matchesSubtree(option, terms) {
  return matchesOption(option, terms) ||
    (option.children?.some((child) => matchesSubtree(child, terms)) ?? false);
}

/**
 * Filter options for a query.
 *
 * At the root level the command core passes the flattened tree (root options
 * plus every submenu descendant). Restricting the candidates to roots keeps
 * the visible list flat, while `matchesSubtree` still lets a query match a
 * nested command. Inside a submenu the options are already the current
 * children, so filtering by `parent` would incorrectly remove every entry.
 *
 * @param {CommandOption[]} options
 * @param {string} query
 * @param {boolean} atRoot
 * @returns {CommandOption[]}
 *
 * @example Empty query leaves options untouched
 * ```js
 * import { filterOptions } from "~/facets/misc/command/common.js";
 *
 * const options = [{ id: "a", label: "Alpha" }];
 *
 * if (filterOptions(options, "", true) !== options) throw new Error("empty query should be an identity");
 * if (filterOptions(options, "   ", true) !== options) throw new Error("whitespace query should be an identity");
 * ```
 *
 * @example Root search keeps only roots but matches descendants
 * ```js
 * import { filterOptions } from "~/facets/misc/command/common.js";
 *
 * const options = [
 *   {
 *     id: "playlists",
 *     label: "Remove playlist",
 *     children: [{ id: "confirm", label: "Confirm: remove Rock" }],
 *   },
 *   { id: "play", label: "Play", parent: "playback" },
 * ];
 *
 * const result = filterOptions(options, "rock", true);
 * if (result.length !== 1 || result[0].id !== "playlists") throw new Error("root should surface for a descendant match");
 * ```
 *
 * @example Submenu search keeps children (which all have a parent)
 * ```js
 * import { filterOptions } from "~/facets/misc/command/common.js";
 *
 * const children = [
 *   { id: "rock", label: "Rock", parent: "playlists" },
 *   { id: "jazz", label: "Jazz", parent: "playlists" },
 * ];
 *
 * const result = filterOptions(children, "roc", false);
 * if (result.length !== 1 || result[0].id !== "rock") throw new Error("submenu children should be searchable");
 * ```
 *
 * @example Disabled options are excluded from query results
 * ```js
 * import { filterOptions } from "~/facets/misc/command/common.js";
 *
 * const options = [
 *   { id: "a", label: "Play", disabled: true },
 *   { id: "b", label: "Pause" },
 * ];
 *
 * const result = filterOptions(options, "p", true);
 * if (result.length !== 1 || result[0].id !== "b") throw new Error("disabled options should be hidden");
 * ```
 */
export function filterOptions(options, query, atRoot) {
  const normalized = query.trim().toLowerCase();
  if (!normalized) return options;

  const terms = normalized.split(/\s+/).filter(Boolean);
  const candidates = atRoot
    ? options.filter((option) => !option.parent)
    : options;

  return candidates.filter(
    (option) => !option.disabled && matchesSubtree(option, terms),
  );
}

/**
 * Compare two sort-field lists.
 *
 * @param {string[] | undefined} a
 * @param {string[]} b
 * @returns {boolean}
 *
 * @example Treats undefined as an empty list
 * ```js
 * import { sameFields } from "~/facets/misc/command/common.js";
 *
 * if (!sameFields(undefined, [])) throw new Error("undefined should equal empty");
 * if (!sameFields(["tags.title"], ["tags.title"])) throw new Error("equal lists should match");
 * if (sameFields(["tags.title"], ["tags.artist"])) throw new Error("different lists should not match");
 * if (sameFields(["tags.title"], ["tags.title", "tags.album"])) throw new Error("prefix should not match");
 * ```
 */
export function sameFields(a, b) {
  return JSON.stringify(a ?? []) === JSON.stringify(b);
}

/**
 * Stable signature of everything that affects the rendered option tree. Used
 * to avoid re-registering options (which resets submenu position) on unrelated
 * signal updates.
 *
 * @param {CommandOption[]} options
 * @returns {string}
 *
 * @example Identical trees produce identical signatures
 * ```js
 * import { signatureOf } from "~/facets/misc/command/common.js";
 *
 * const tree = [
 *   {
 *     id: "a",
 *     label: "Alpha",
 *     group: "Group",
 *     data: { checked: true },
 *     children: [{ id: "b", label: "Beta" }],
 *   },
 * ];
 *
 * const copy = [
 *   {
 *     id: "a",
 *     label: "Alpha",
 *     group: "Group",
 *     data: { checked: true },
 *     children: [{ id: "b", label: "Beta" }],
 *   },
 * ];
 *
 * if (signatureOf(tree) !== signatureOf(copy)) throw new Error("equal trees should have equal signatures");
 * ```
 *
 * @example Label, disabled, and checked changes alter the signature
 * ```js
 * import { signatureOf } from "~/facets/misc/command/common.js";
 *
 * const base = [{ id: "a", label: "Play" }];
 * const renamed = [{ id: "a", label: "Pause" }];
 * const disabled = [{ id: "a", label: "Play", disabled: true }];
 * const checked = [{ id: "a", label: "Play", data: { checked: true } }];
 * const child = [{ id: "a", label: "Play", children: [{ id: "b", label: "Beta" }] }];
 *
 * if (signatureOf(base) === signatureOf(renamed)) throw new Error("label change should alter signature");
 * if (signatureOf(base) === signatureOf(disabled)) throw new Error("disabled change should alter signature");
 * if (signatureOf(base) === signatureOf(checked)) throw new Error("checked change should alter signature");
 * if (signatureOf(base) === signatureOf(child)) throw new Error("child change should alter signature");
 * ```
 */
export function signatureOf(options) {
  /** @param {CommandOption} option @returns {unknown} */
  const serialize = (option) => [
    option.id,
    option.label,
    option.group ?? "",
    option.disabled ? 1 : 0,
    option.data?.checked ? 1 : 0,
    option.children?.map(serialize),
  ];

  return JSON.stringify(options.map(serialize));
}

/**
 * Human-readable label for the currently playing track.
 *
 * @param {Track | null | undefined} track
 * @returns {string}
 *
 * @example Formats artist and title, with graceful fallbacks
 * ```js
 * import { trackLabel } from "~/facets/misc/command/common.js";
 *
 * // @ts-ignore
 * const both = trackLabel({ tags: { artist: "Artist", title: "Song" } });
 * if (both !== "Artist - Song") throw new Error("expected artist - title");
 *
 * // @ts-ignore
 * const title = trackLabel({ tags: { title: "Song" } });
 * if (title !== "Song") throw new Error("expected title fallback");
 *
 * if (trackLabel(null) !== "current track") throw new Error("expected generic fallback");
 * ```
 */
export function trackLabel(track) {
  if (!track) return "current track";

  const artist = track.tags?.artist;
  const title = track.tags?.title;

  if (artist && title) return `${artist} - ${title}`;
  return title || artist || "current track";
}

/**
 * Build playlist criteria for a track. Artist + title is preferred so copies of
 * the same recording across sources all match; otherwise fall back to the URI.
 *
 * @param {Track} track
 * @returns {PlaylistItem["criteria"]}
 *
 * @example Uses artist and title with lowercase transformations
 * ```js
 * import { criteriaFor } from "~/facets/misc/command/common.js";
 *
 * // @ts-ignore
 * const criteria = criteriaFor({ uri: "https://example.com/a.mp3", tags: { artist: "Artist", title: "Song" } });
 *
 * if (criteria.length !== 2) throw new Error("expected two criteria");
 * if (criteria[0].field !== "tags.artist") throw new Error("expected artist criterion");
 * if (criteria[1].field !== "tags.title") throw new Error("expected title criterion");
 * if (criteria[0].transformations?.[0] !== "toLowerCase") throw new Error("expected lowercase transformation");
 * ```
 *
 * @example Falls back to the exact URI when tags are incomplete
 * ```js
 * import { criteriaFor } from "~/facets/misc/command/common.js";
 *
 * // @ts-ignore
 * const criteria = criteriaFor({ uri: "https://example.com/a.mp3", tags: { title: "Song" } });
 *
 * if (criteria.length !== 1) throw new Error("expected one criterion");
 * if (criteria[0].field !== "uri") throw new Error("expected uri criterion");
 * if (String(criteria[0].value) !== "https://example.com/a.mp3") throw new Error("expected exact uri");
 * ```
 */
export function criteriaFor(track) {
  const artist = track.tags?.artist;
  const title = track.tags?.title;

  if (artist && title) {
    return [
      {
        field: "tags.artist",
        value: /** @type {Record<string, unknown>} */ (
          /** @type {unknown} */ (artist)
        ),
        transformations: ["toLowerCase"],
      },
      {
        field: "tags.title",
        value: /** @type {Record<string, unknown>} */ (
          /** @type {unknown} */ (title)
        ),
        transformations: ["toLowerCase"],
      },
    ];
  }

  return [{
    field: "uri",
    value: /** @type {Record<string, unknown>} */ (
      /** @type {unknown} */ (track.uri)
    ),
  }];
}
