/**
 * Cooperative playlists: helpers for working with `sh.diffuse.output.collaboration`
 * records and for combining playlist items contributed by multiple accounts.
 *
 * A collaboration record says which identities share one of *our* playlists.
 * There is no central authority: every account decides for itself who is in the
 * club, and a cohesive playlist is assembled by collecting playlist items from
 * every reachable collaborator and grouping them by playlist name.
 *
 * @import {Collaboration, Collaborator, PlaylistItem} from "~/definitions/types.d.ts"
 */

import * as TID from "@atcute/tid";

export const TYPE = /** @type {const} */ ("sh.diffuse.output.collaboration");

/**
 * A playlist item contributed by another account.
 *
 * Collaborator items are kept in memory only: their `id` is scoped with the
 * owner's DID so it can never collide with a local item, and `$owner` marks the
 * item so it can be filtered out on save. None of the `$`-prefixed properties
 * are persisted.
 *
 * @typedef {PlaylistItem & {
 *   $owner?: string;
 *   $ownerLabel?: string;
 *   $sourceId?: string;
 * }} ScopedPlaylistItem
 */

/**
 * The set of items contributed by one collaborator.
 *
 * @typedef {{
 *   playlist: string;
 *   owner: string;
 *   label?: string;
 *   items: PlaylistItem[];
 * }} RemotePlaylistGroup
 */

/**
 * @param {Collaboration} collaboration
 * @returns {string | undefined}
 */
function playlistOf(collaboration) {
  return collaboration.subject?.kind === "playlist"
    ? collaboration.subject.playlist
    : undefined;
}

/**
 * Find the first collaboration record for a playlist.
 *
 * @param {Collaboration[]} collaborations
 * @param {string} playlist
 * @returns {Collaboration | undefined}
 *
 * @example Finds the record for a playlist
 * ```js
 * import { findCollaboration } from "~/common/collaboration/index.js";
 *
 * const record = JSON.parse('{"$type":"sh.diffuse.output.collaboration","id":"c1","subject":{"kind":"playlist","playlist":"Favourites"},"collaborators":[]}');
 * if (findCollaboration([record], "Favourites")?.id !== "c1") throw new Error("expected record");
 * if (findCollaboration([record], "Other") !== undefined) throw new Error("expected undefined");
 * ```
 */
export function findCollaboration(collaborations, playlist) {
  return collaborations.find((collaboration) =>
    playlistOf(collaboration) === playlist
  );
}

/**
 * Collect the unique collaborators of a playlist across every matching record.
 *
 * @param {Collaboration[]} collaborations
 * @param {string} playlist
 * @returns {Collaborator[]}
 *
 * @example Merges and deduplicates collaborators from multiple records
 * ```js
 * import { collaboratorsFor } from "~/common/collaboration/index.js";
 *
 * const collaborators = collaboratorsFor(JSON.parse('[{"$type":"sh.diffuse.output.collaboration","id":"c1","subject":{"kind":"playlist","playlist":"Mix"},"collaborators":[{"id":"alice.test","hint":"atproto/handle"},{"id":"alice.test","hint":"atproto/handle"},{"id":"did:plc:bob","hint":"atproto/did"}]}]'), "Mix");
 * if (collaborators.length !== 2) throw new Error("expected deduplicated collaborators");
 * ```
 */
export function collaboratorsFor(collaborations, playlist) {
  /** @type {Map<string, Collaborator>} */
  const byId = new Map();

  for (const collaboration of collaborations) {
    if (playlistOf(collaboration) !== playlist) continue;

    for (const collaborator of collaboration.collaborators ?? []) {
      const id = collaborator.id?.trim();
      if (!id || byId.has(id)) continue;
      byId.set(id, { ...collaborator, id });
    }
  }

  return [...byId.values()];
}

/**
 * List every playlist that has collaborators and who they are.
 *
 * @param {Collaboration[]} collaborations
 * @returns {Map<string, Collaborator[]>}
 *
 * @example Groups collaborators by playlist name
 * ```js
 * import { collaborationPlaylists } from "~/common/collaboration/index.js";
 *
 * const map = collaborationPlaylists(JSON.parse('[{"$type":"sh.diffuse.output.collaboration","id":"c1","subject":{"kind":"playlist","playlist":"Mix"},"collaborators":[{"id":"alice.test","hint":"atproto/handle"}]}]'));
 * if (map.get("Mix")?.length !== 1) throw new Error("expected Mix collaborators");
 * ```
 */
export function collaborationPlaylists(collaborations) {
  /** @type {Map<string, Collaborator[]>} */
  const map = new Map();
  /** @type {Map<string, Set<string>>} */
  const seen = new Map();

  for (const collaboration of collaborations) {
    const playlist = playlistOf(collaboration);
    if (!playlist) continue;

    const collaborators = map.get(playlist) ?? [];
    const ids = seen.get(playlist) ?? new Set();

    for (const collaborator of collaboration.collaborators ?? []) {
      const id = collaborator.id?.trim();
      if (!id || ids.has(id)) continue;
      ids.add(id);
      collaborators.push({ ...collaborator, id });
    }

    map.set(playlist, collaborators);
    seen.set(playlist, ids);
  }

  return map;
}

/**
 * Expand a collaboration graph by following collaborators' own collaboration
 * records. Only playlists already present in the root set are extended, so a
 * collaborator cannot introduce us to entirely new playlists.
 *
 * The `load` callback receives a collaborator and returns that collaborator's
 * collaboration records. It is only called for collaborators accepted by
 * `isSupported` (by default, every collaborator). Protocol availability is the
 * caller's concern, usually via the source registry.
 *
 * @param {Collaboration[]} collaborations
 * @param {{
 *   depth?: number;
 *   load?: (collaborator: Collaborator) => Promise<Collaboration[]>;
 *   isSupported?: (collaborator: Collaborator) => boolean;
 * }} [options]
 * @returns {Promise<Map<string, Collaborator[]>>}
 *
 * @example Follows collaborators one level deep
 * ```js
 * import { expandCollaborators } from "~/common/collaboration/index.js";
 *
 * const root = [{
 *   $type: "sh.diffuse.output.collaboration",
 *   id: "root",
 *   subject: { kind: "playlist", playlist: "Mix" },
 *   collaborators: [{ id: "alice.test", hint: "atproto/handle" }],
 * }];
 *
 * const remote = [{
 *   $type: "sh.diffuse.output.collaboration",
 *   id: "alice",
 *   subject: { kind: "playlist", playlist: "Mix" },
 *   collaborators: [{ id: "bob.test", hint: "atproto/handle" }],
 * }];
 *
 * // @ts-ignore
 * const graph = await expandCollaborators(root, {
 *   depth: 2,
 *   // @ts-ignore
 *   load: async () => remote,
 * });
 * if (graph.get("Mix")?.length !== 2) throw new Error("expected transitive collaborator");
 * ```
 *
 * @example Does not recurse at depth 1
 * ```js
 * import { expandCollaborators } from "~/common/collaboration/index.js";
 *
 * const root = [{
 *   $type: "sh.diffuse.output.collaboration",
 *   id: "root",
 *   subject: { kind: "playlist", playlist: "Mix" },
 *   collaborators: [{ id: "alice.test", hint: "atproto/handle" }],
 * }];
 * // @ts-ignore
 * const graph = await expandCollaborators(root, { depth: 1, load: async () => [] });
 * if (graph.get("Mix")?.length !== 1) throw new Error("expected only direct collaborator");
 * ```
 */
export async function expandCollaborators(collaborations, options = {}) {
  const {
    depth = 1,
    load,
    isSupported = () => true,
  } = options;

  /** @type {Map<string, Collaborator[]>} */
  const direct = collaborationPlaylists(collaborations);
  if (depth <= 1 || !load) return direct;

  /** @type {Map<string, Map<string, Collaborator>>} */
  const graph = new Map();
  for (const [playlist, collaborators] of direct) {
    graph.set(playlist, new Map(collaborators.map((c) => [c.id, c])));
  }

  /** @type {{ playlist: string; collaborator: Collaborator }[]} */
  let frontier = [];
  for (const [playlist, collaborators] of direct) {
    for (const collaborator of collaborators) {
      frontier.push({ playlist, collaborator });
    }
  }

  const visited = new Set();

  for (let level = 1; level < depth; level++) {
    /** @type {{ playlist: string; collaborator: Collaborator }[]} */
    const next = [];

    /** @type {Map<string, { collaborator: Collaborator; playlists: Set<string> }>} */
    const byActor = new Map();
    for (const entry of frontier) {
      if (!isSupported(entry.collaborator)) continue;

      const existing = byActor.get(entry.collaborator.id);
      if (existing) {
        existing.playlists.add(entry.playlist);
      } else {
        byActor.set(entry.collaborator.id, {
          collaborator: entry.collaborator,
          playlists: new Set([entry.playlist]),
        });
      }
    }

    for (const [id, { collaborator, playlists }] of byActor) {
      if (visited.has(id)) continue;
      visited.add(id);

      /** @type {Collaboration[]} */
      let records;
      try {
        records = await load(collaborator);
      } catch (err) {
        console.warn(`Could not expand collaborators of ${id}:`, err);
        continue;
      }

      for (const record of records) {
        const playlist = playlistOf(record);
        if (!playlist || !playlists.has(playlist)) continue;

        const bucket = graph.get(playlist);
        if (!bucket) continue;

        for (const nextCollaborator of record.collaborators ?? []) {
          const nextId = nextCollaborator.id?.trim();
          if (!nextId || bucket.has(nextId)) continue;
          bucket.set(nextId, { ...nextCollaborator, id: nextId });
          next.push({
            playlist,
            collaborator: { ...nextCollaborator, id: nextId },
          });
        }
      }
    }

    frontier = next;
    if (frontier.length === 0) break;
  }

  return new Map(
    [...graph].map(([playlist, collaborators]) => [
      playlist,
      [...collaborators.values()],
    ]),
  );
}

/**
 * Replace the collaborators of a playlist, creating or removing the
 * collaboration record as needed. Duplicate records for the same playlist are
 * collapsed into one.
 *
 * @param {Collaboration[]} collaborations
 * @param {string} playlist
 * @param {Collaborator[]} collaborators
 * @returns {Collaboration[]}
 *
 * @example Creates a record when none exists
 * ```js
 * import { setCollaborators } from "~/common/collaboration/index.js";
 *
 * const result = setCollaborators([], "Mix", [
 *   { id: "alice.test", hint: "atproto/handle" },
 * ]);
 * if (result.length !== 1) throw new Error("expected one record");
 * if (result[0].subject.playlist !== "Mix") throw new Error("expected playlist subject");
 * ```
 *
 * @example Removes the record when the collaborator list becomes empty
 * ```js
 * import { setCollaborators } from "~/common/collaboration/index.js";
 *
 * const existing = JSON.parse('[{"$type":"sh.diffuse.output.collaboration","id":"c1","subject":{"kind":"playlist","playlist":"Mix"},"collaborators":[{"id":"alice.test","hint":"atproto/handle"}]}]');
 * if (setCollaborators(existing, "Mix", []).length !== 0) throw new Error("expected removal");
 * ```
 */
export function setCollaborators(collaborations, playlist, collaborators) {
  /** @type {Map<string, Collaborator>} */
  const byId = new Map();
  for (const collaborator of collaborators) {
    const id = collaborator.id?.trim();
    if (!id) continue;
    byId.set(id, { ...collaborator, id });
  }

  const cleaned = [...byId.values()];
  const matching = collaborations.filter((c) => playlistOf(c) === playlist);
  const untouched = collaborations.filter((c) => playlistOf(c) !== playlist);

  if (cleaned.length === 0) return untouched;

  const now = new Date().toISOString();
  const first = matching[0];

  if (!first) {
    return [...untouched, {
      $type: TYPE,
      id: TID.now(),
      subject: {
        $type: "sh.diffuse.output.collaboration#playlist",
        kind: "playlist",
        playlist,
      },
      collaborators: cleaned,
      createdAt: now,
      updatedAt: now,
    }];
  }

  return [...untouched, { ...first, collaborators: cleaned, updatedAt: now }];
}

/**
 * Scope a collaborator's playlist items so they can be merged with local items
 * without id collisions.
 *
 * @param {string} owner - Stable identity of the collaborator (e.g. their DID).
 * @param {PlaylistItem[]} items
 * @param {string} [label] - Human-readable label (e.g. their handle).
 * @returns {ScopedPlaylistItem[]}
 *
 * @example Prefixes ids and rewrites intra-owner positionedAfter links
 * ```js
 * import { scopeItems } from "~/common/collaboration/index.js";
 *
 * const items = JSON.parse('[{"$type":"sh.diffuse.output.playlistItem","id":"a","playlist":"Mix","criteria":[]},{"$type":"sh.diffuse.output.playlistItem","id":"b","playlist":"Mix","criteria":[],"positionedAfter":"a"}]');
 * const scoped = scopeItems("did:plc:alice", items, "alice.test");
 * if (scoped[0].id !== "did:plc:alice/a") throw new Error("expected scoped id");
 * if (scoped[1].positionedAfter !== "did:plc:alice/a") throw new Error("expected scoped link");
 * if (scoped[0].$owner !== "did:plc:alice") throw new Error("expected owner");
 * ```
 */
export function scopeItems(owner, items, label = owner) {
  const ids = new Set(items.map((item) => item.id));

  return items.map((item) => ({
    ...item,
    id: `${owner}/${item.id}`,
    $owner: owner,
    $ownerLabel: label,
    $sourceId: item.id,
    positionedAfter: item.positionedAfter
      ? ids.has(item.positionedAfter)
        ? `${owner}/${item.positionedAfter}`
        : item.positionedAfter
      : undefined,
  }));
}

/**
 * Whether an item was contributed by a collaborator.
 *
 * @param {PlaylistItem} item
 * @returns {item is ScopedPlaylistItem}
 *
 * @example Detects collaborator items
 * ```js
 * import { isRemoteItem } from "~/common/collaboration/index.js";
 *
 * if (isRemoteItem(JSON.parse('{"$type":"sh.diffuse.output.playlistItem","id":"a","playlist":"p","criteria":[],"$owner":"did:plc:x"}'))) {
 *   // remote
 * } else {
 *   throw new Error("expected remote item");
 * }
 * if (isRemoteItem(JSON.parse('{"$type":"sh.diffuse.output.playlistItem","id":"a","playlist":"p","criteria":[]}'))) {
 *   throw new Error("local item should not be remote");
 * }
 * ```
 */
export function isRemoteItem(item) {
  return typeof (/** @type {ScopedPlaylistItem} */ (item).$owner) === "string";
}

/**
 * @param {PlaylistItem} item
 * @returns {string}
 */
function signature(item) {
  return `${item.playlist}\0${item.id}\0${JSON.stringify(item.criteria)}`;
}

/**
 * Combine local playlist items with the items contributed by collaborators.
 *
 * Remote items are scoped by owner, deduplicated, and omitted when an identical
 * item already exists locally (which happens when we fetch our own repo).
 *
 * @param {PlaylistItem[]} localItems
 * @param {RemotePlaylistGroup[]} remoteGroups
 * @returns {ScopedPlaylistItem[]}
 *
 * @example Appends scoped remote items and skips local duplicates
 * ```js
 * import { mergeItems } from "~/common/collaboration/index.js";
 *
 * const local = JSON.parse('[{"$type":"sh.diffuse.output.playlistItem","id":"a","playlist":"Mix","criteria":[{"field":"uri","value":"x"}]}]');
 * const merged = mergeItems(local, JSON.parse('[{"playlist":"Mix","owner":"did:plc:alice","label":"alice.test","items":[{"$type":"sh.diffuse.output.playlistItem","id":"a","playlist":"Mix","criteria":[{"field":"uri","value":"x"}]},{"$type":"sh.diffuse.output.playlistItem","id":"b","playlist":"Mix","criteria":[{"field":"uri","value":"y"}]}]}]'));
 * if (merged.length !== 2) throw new Error("expected local + one remote item");
 * if (merged[1].$owner !== "did:plc:alice") throw new Error("expected remote item");
 * ```
 */
export function mergeItems(localItems, remoteGroups) {
  const localSignatures = new Set(localItems.map(signature));
  const seen = new Set();
  /** @type {ScopedPlaylistItem[]} */
  const merged = [...localItems];

  for (const group of remoteGroups) {
    for (const item of scopeItems(group.owner, group.items, group.label)) {
      // Generated/unsaved items from a collaborator's output are not shared.
      if (item.ephemeral) continue;

      const key = `${group.owner}\0${item.$sourceId}`;
      if (seen.has(key)) continue;
      if (
        localSignatures.has(
          signature({ ...item, id: item.$sourceId ?? item.id }),
        )
      ) {
        continue;
      }

      seen.add(key);
      merged.push(item);
    }
  }

  return merged;
}
