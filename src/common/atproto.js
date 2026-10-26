/**
 * Small, shared AT Protocol helpers for resolving identities and reading
 * records/collections from other accounts. Used by cooperative playlists to
 * load collaborators' playlist items and collaboration records.
 *
 * @import {Collaboration, PlaylistItem} from "~/definitions/types.d.ts"
 */

import { Client, ok, simpleFetchHandler } from "@atcute/client";
import { decode } from "@atcute/cbor";
import {
  CompositeDidDocumentResolver,
  LocalActorResolver,
  PlcDidDocumentResolver,
  WebDidDocumentResolver,
  XrpcHandleResolver,
} from "@atcute/identity-resolver";

import { decodeCollection } from "~/common/lens.js";

export const COLLABORATION = "sh.diffuse.output.collaboration";
export const PLAYLIST_ITEM = "sh.diffuse.output.playlistItem";
export const PLAYLIST_ITEM_BUNDLE = "sh.diffuse.output.playlistItemBundle";

const resolver = new LocalActorResolver({
  handleResolver: new XrpcHandleResolver({
    serviceUrl: "https://public.api.bsky.app",
  }),
  didDocumentResolver: new CompositeDidDocumentResolver({
    methods: {
      plc: new PlcDidDocumentResolver(),
      web: new WebDidDocumentResolver(),
    },
  }),
});

/** @typedef {Awaited<ReturnType<typeof resolver.resolve>>} ActorIdentity */

/** @type {Map<string, Promise<ActorIdentity>>} */
const identities = new Map();

/** @type {Map<string, ReturnType<typeof newClient>>} */
const clients = new Map();

/**
 * @param {ActorIdentity} identity
 */
function newClient(identity) {
  return new Client({
    handler: simpleFetchHandler({ service: identity.pds }),
  });
}

/**
 * Resolve a handle or DID to its identity document, including the PDS URL.
 *
 * @param {string} actor
 * @returns {Promise<ActorIdentity>}
 */
export async function resolveActor(actor) {
  const existing = identities.get(actor);
  if (existing) return existing;

  const identity = resolver.resolve(
    /** @type {import("@atcute/lexicons/syntax").ActorIdentifier} */ (actor),
  ).catch((err) => {
    identities.delete(actor);
    throw err;
  });

  identities.set(actor, identity);
  return identity;
}

/**
 * Get a cached client for an identity.
 *
 * @param {ActorIdentity} identity
 */
export function clientFor(identity) {
  const key = `${identity.did}\0${identity.pds}`;
  const existing = clients.get(key);
  if (existing) return existing;

  const client = newClient(identity);
  clients.set(key, client);
  return client;
}

/**
 * Resolve an `at://` URI to the record value stored at that address.
 *
 * @param {string} uri
 * @returns {Promise<any>}
 */
export async function loadAtProtoRecord(uri) {
  const parts = uri.replace(/at:\/\//, "").split("/");
  const [repo, collection, rkey] = parts;

  if (!repo || !collection || !rkey) {
    throw new Error(`Invalid at:// URI: ${uri}`);
  }

  const identity = await resolveActor(repo);
  const rpc = clientFor(identity);

  /** @type {any} */
  const { value } = await ok(
    /** @type {any} */ (rpc).get("com.atproto.repo.getRecord", {
      params: { repo: identity.did, collection, rkey },
    }),
  );

  return value;
}

/**
 * List every record in one collection of an account.
 *
 * @param {string} actor - Handle or DID.
 * @param {string} collection - Collection NSID.
 * @returns {Promise<{ identity: ActorIdentity; records: any[] }>}
 */
export async function listAtProtoRecords(actor, collection) {
  const identity = await resolveActor(actor);
  const rpc = clientFor(identity);

  /** @type {any[]} */
  const records = [];
  /** @type {string | undefined} */
  let cursor;

  do {
    const page = /** @type {{ records: { value: any }[]; cursor?: string }} */ (
      await ok(
        /** @type {any} */ (rpc).get("com.atproto.repo.listRecords", {
          params: {
            repo: identity.did,
            collection:
              /** @type {`${string}.${string}.${string}`} */ (collection),
            limit: 100,
            cursor,
          },
        }),
      )
    );

    records.push(...page.records.map((record) => record.value));
    cursor = page.cursor;
  } while (cursor);

  return { identity, records };
}

/**
 * Fetch a blob from an account's PDS.
 *
 * @param {ActorIdentity} identity
 * @param {string} cid
 * @returns {Promise<Uint8Array>}
 */
export async function getAtProtoBlob(identity, cid) {
  const rpc = clientFor(identity);
  return /** @type {Uint8Array} */ (await ok(
    /** @type {any} */ (rpc).get("com.atproto.sync.getBlob", {
      params: { did: identity.did, cid },
      as: "bytes",
    }),
  ));
}

/**
 * Read a collaborator's playlists items for one playlist.
 *
 * Both storage shapes are supported: bundle records containing a CBOR blob of
 * playlist items (the shape Diffuse writes), and individual playlist item
 * records (useful for other clients).
 *
 * @param {string} actor - Handle or DID.
 * @param {string} playlist
 * @returns {Promise<{ identity: ActorIdentity; items: PlaylistItem[] }>}
 */
export async function loadPlaylistItems(actor, playlist) {
  const { identity, records: bundles } = await listAtProtoRecords(
    actor,
    PLAYLIST_ITEM_BUNDLE,
  );

  /** @type {PlaylistItem[]} */
  const items = [];

  for (const bundle of bundles) {
    if (bundle?.playlist !== playlist) continue;

    const cid = bundle?.data?.ref?.$link;
    if (typeof cid !== "string") continue;

    try {
      const bytes = await getAtProtoBlob(identity, cid);
      const decoded = decodeCollection(decode(bytes), "playlistItems");
      if (!Array.isArray(decoded)) continue;
      items.push(.../** @type {PlaylistItem[]} */ (decoded));
    } catch (err) {
      console.warn(`Could not read playlist bundle for ${actor}:`, err);
    }
  }

  // Also support clients that store each item as its own record.
  try {
    const { records } = await listAtProtoRecords(identity.did, PLAYLIST_ITEM);
    for (const record of records) {
      if (record?.playlist === playlist) {
        items.push(/** @type {PlaylistItem} */ (record));
      }
    }
  } catch {
    // The collection may simply not exist; that's fine.
  }

  return { identity, items: dedupeById(items) };
}

/**
 * Read a collaborator's collaboration records.
 *
 * @param {string} actor - Handle or DID.
 * @returns {Promise<Collaboration[]>}
 */
export async function loadCollaborations(actor) {
  const { records } = await listAtProtoRecords(actor, COLLABORATION);
  return /** @type {Collaboration[]} */ (records);
}

/**
 * @param {PlaylistItem[]} items
 * @returns {PlaylistItem[]}
 */
function dedupeById(items) {
  /** @type {Map<string, PlaylistItem>} */
  const byId = new Map();
  for (const item of items) {
    if (typeof item?.id !== "string") continue;
    byId.set(item.id, item);
  }
  return [...byId.values()];
}
