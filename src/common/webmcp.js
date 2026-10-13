import foundation from "~/common/foundation.js";
import { effect } from "~/common/signal.js";
import * as TID from "@atcute/tid";
import * as Output from "~/common/output.js";
import { resolveFacetTile } from "~/common/loader.js";
import {
  filterByPlaylist,
  gather,
  match as matchPlaylistItem,
  orderByPlaylist,
  sort as sortPlaylistItems,
} from "~/common/playlist.js";
import { groupTracksPerScheme } from "~/common/utils.js";
import { isAudioFile } from "~/components/input/common.js";
import {
  htmlFacetTile,
  tileFromFiles,
  tileResourceEntries,
} from "~/common/tiles.js";

/**
 * @import {Facet, PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

const FACET_TYPE = "sh.diffuse.output.facet";
const PLAYLIST_ITEM_TYPE = "sh.diffuse.output.playlistItem";

/** The catalogue URI of the WebMCP facet itself. */
export const WEBMCP_FACET_URI = "diffuse://facets/misc/webmcp/prelude/index.tile";

/** @param {string} text */
const ok = (text) => ({ content: [{ type: "text", text }] });

////////////////////////////////////////////
// DOCS
////////////////////////////////////////////

// The facet development documentation is bundled with the Diffuse build (the
// same content the offline `diffuse-facet` agent skill ships), fetched relative
// to the build root — the loader page's `<base>` element makes this resolve.
const DOCS_BASE = "skills/diffuse-facet/";

const DOCS = [
  {
    path: "SKILL.md",
    description:
      "Start here: the step-by-step guide for creating and editing facets — mandatory rules, skeleton, reactivity, audio control.",
  },
  {
    path: "docs/architecture.txt",
    description:
      "System overview: components, definitions, facets and the foundation.",
  },
  {
    path: "docs/elements.txt",
    description:
      "Every available custom element (artwork, engines, orchestrators, output, transformers, …) with its description and full source. Large: read without 'query' for the catalog, then use 'query' to read specific elements.",
  },
  {
    path: "docs/foundation.js",
    description:
      "The foundation API source: element accessors, signals and configuration.",
  },
  {
    path: "example/index.html",
    description:
      "A representative interface facet to use as a reference.",
  },
  {
    path: "docs/definitions/index.ts",
    description:
      "TypeScript types for all data structures (Facet, Track, PlaylistItem, Setting, …). Individual schemas live under docs/definitions/, e.g. docs/definitions/output/track.json for the Track lexicon.",
  },
];

/** @param {string} path */
async function fetchDoc(path) {
  try {
    const res = await fetch(DOCS_BASE + path.replace(/^\/+/, ""));
    if (!res.ok) return undefined;
    return await res.text();
  } catch {
    return undefined;
  }
}

/**
 * The elements documentation carries every component's full source and is far
 * too large to return at once. Without a query, return just the catalog (every
 * element's name and group); with one, return the matching sections.
 *
 * @param {string} text
 * @param {string | undefined} query
 */
function elementsDoc(text, query) {
  /** @type {{ group: string; name: string; body: string }[]} */
  const sections = [];

  let group = "";
  /** @type {{ group: string; name: string; body: string } | undefined} */
  let current;
  for (const line of text.split("\n")) {
    if (line.startsWith("## ")) {
      group = line.slice(3).trim();
      current = undefined;
    } else if (line.startsWith("### ")) {
      current = { group, name: line.slice(4).trim(), body: "" };
      sections.push(current);
    } else if (current) {
      current.body += line + "\n";
    }
  }

  if (!query) {
    return JSON.stringify({
      total: sections.length,
      note:
        "Call again with 'query' to read the matching elements' description " +
        "and source code, e.g. query: \"queue\" or query: \"browser\".",
      elements: sections.map((s) => ({ group: s.group, name: s.name })),
    }, null, 2);
  }

  const q = query.trim().toLowerCase();
  const matches = sections.filter((s) =>
    s.name.toLowerCase().includes(q) || s.body.toLowerCase().includes(q)
  );

  if (!matches.length) {
    return `No elements match "${query}". Call without 'query' for the catalog.`;
  }

  // Sections can be sizeable (each embeds the element's full source), so cap
  // the amount returned per call.
  const max = 5;
  const doc = matches.slice(0, max)
    .map((s) => `## ${s.group}\n### ${s.name}\n${s.body.trimEnd()}`)
    .join("\n\n");

  return matches.length > max
    ? `${doc}\n\n(${matches.length - max} more matches — refine 'query'.)`
    : doc;
}

////////////////////////////////////////////
// FACETS
////////////////////////////////////////////

/** @returns {Promise<Facet[]>} */
async function listFacets() {
  const out = await foundation.orchestrator.output();
  return /** @type {Facet[]} */ (await Output.data(out.facets));
}

/** @param {Facet} facet */
async function saveFacet(facet) {
  const out = await foundation.orchestrator.output();
  const col = await Output.data(out.facets);
  const withoutId = col.filter((c) => c.id !== facet.id);
  await out.facets.save([...withoutId, {
    ...facet,
    updatedAt: new Date().toISOString(),
  }]);
}

/**
 * Where a facet's code lives: inline in the record ("custom"), the Diffuse
 * bundle ("bundle"), or an external URI ("external"). Mirrors the dashboard's
 * "custom code" vs "tracking the original URI" distinction.
 *
 * @param {Facet} facet
 */
function facetSource(facet) {
  if (facet.uri && !facet.resources) {
    return facet.uri.startsWith("diffuse://") ? "bundle" : "external";
  }
  return "custom";
}

/** @param {Facet} facet */
function facetSummary(facet) {
  return {
    id: facet.id,
    name: facet.name,
    kind: facet.kind ?? "interactive",
    description: facet.description ?? "",
    enabled: facet.enabled ?? true,
    favourite: facet.favourite ?? false,
    source: facetSource(facet),
    uri: facet.uri,
    tags: facet.tags,
  };
}

/**
 * Resolves a facet's tile files (every resource's path and text content), or
 * undefined when the facet's content can't be resolved. Facets that don't ship
 * a tile resolve as a single `/` file.
 *
 * @param {Facet} facet
 * @returns {Promise<{ path: string; content: string }[] | undefined>}
 */
async function facetFiles(facet) {
  const tile = await resolveFacetTile(facet);
  if (!tile) return undefined;

  const decoder = new TextDecoder();
  const entries = [...tileResourceEntries(tile.resources, tile.blocks)]
    .map(([path, entry]) => ({ path, content: decoder.decode(entry.bytes) }));

  if (!entries.length) return [{ path: "/", content: tile.html }];

  return entries.sort((a, b) =>
    a.path === "/" ? -1 : b.path === "/" ? 1 : a.path.localeCompare(b.path)
  );
}

////////////////////////////////////////////
// TRACKS & PLAYLISTS
////////////////////////////////////////////

/** @returns {Promise<Track[]>} */
async function collectionTracks() {
  const out = await foundation.orchestrator.output();
  return /** @type {Track[]} */ (await Output.data(out.tracks));
}

/** @returns {Promise<PlaylistItem[]>} */
async function collectionPlaylistItems() {
  const out = await foundation.orchestrator.output();
  return /** @type {PlaylistItem[]} */ (await Output.data(out.playlistItems));
}

/**
 * @param {Track | undefined} track
 * @returns {string}
 */
const trackArtist = (track) =>
  track?.tags?.artist ?? track?.tags?.albumartist ?? "";

/** @param {Track | undefined} track */
const trackAlbum = (track) => track?.tags?.album ?? "";

/** @param {Track | undefined} track */
const trackTitle = (track) => track?.tags?.title ?? "";

/** @param {Track | undefined} track */
const trackSummary = (track) => ({
  id: track?.id,
  title: trackTitle(track),
  artist: trackArtist(track),
  album: trackAlbum(track),
  // Milliseconds, as stored on the track's stats.
  duration: track?.stats?.duration,
});

/** @param {Track} track */
const trackHaystack = (track) => [
  trackTitle(track),
  trackArtist(track),
  ...(track?.tags?.artists ?? []),
  track?.tags?.albumartist,
  trackAlbum(track),
].filter(Boolean).join("\n").toLowerCase();

/**
 * @param {Track[]} tracks
 * @returns {Track[]}
 */
function sortTracks(tracks) {
  return [...tracks].sort((a, b) =>
    trackArtist(a).localeCompare(trackArtist(b)) ||
    trackAlbum(a).localeCompare(trackAlbum(b)) ||
    trackTitle(a).localeCompare(trackTitle(b)) ||
    a.id.localeCompare(b.id)
  );
}

/**
 * @param {Track[]} tracks
 * @param {string[]} trackIds
 * @returns {string[]} ids that don't exist in the collection
 */
function unknownTrackIds(tracks, trackIds) {
  const known = new Set(tracks.map((t) => t.id));
  return [...new Set(trackIds)].filter((id) => !known.has(id));
}

/**
 * Resolves track ids against the collection.
 *
 * @param {string[]} trackIds
 * @returns {Promise<{ tracks: Track[]; unknown: string[] }>}
 */
async function resolveTrackIds(trackIds) {
  const tracks = await collectionTracks();
  const byId = new Map(tracks.map((t) => [t.id, t]));

  const resolved = [...new Set(trackIds)]
    .map((id) => byId.get(id))
    .filter((t) => !!t);

  return { tracks: resolved, unknown: unknownTrackIds(tracks, trackIds) };
}

/**
 * Finds a playlist's items by name (exact first, then case-insensitive).
 *
 * @param {PlaylistItem[]} items
 * @param {string} name
 * @returns {PlaylistItem[]}
 */
function findPlaylistItems(items, name) {
  let playlistItems = items.filter((i) => i.playlist === name);
  if (!playlistItems.length) {
    const lower = name.toLowerCase();
    playlistItems = items.filter((i) => i.playlist.toLowerCase() === lower);
  }
  return playlistItems;
}

/**
 * Builds a per-track playlist item — the same shape the favourites
 * orchestrator uses (artist + title, lowercased, so matching is forgiving).
 *
 * @param {string} playlist
 * @param {Track} track
 * @param {string | undefined} positionedAfter
 * @returns {PlaylistItem}
 */
function trackPlaylistItem(playlist, track, positionedAfter) {
  const transformations = ["toLowerCase"];
  const now = new Date().toISOString();

  // The generated Criterion type types `value` narrowly (the favourites
  // orchestrator's own builder casts for the same reason).
  return /** @type {PlaylistItem} */ ({
    $type: PLAYLIST_ITEM_TYPE,
    id: TID.now(),
    playlist,
    criteria: [
      {
        field: "tags.artist",
        value: /** @type {unknown} */ (track?.tags?.artist),
        transformations,
      },
      {
        field: "tags.title",
        value: /** @type {unknown} */ (track?.tags?.title),
        transformations,
      },
    ],
    ...(positionedAfter ? { positionedAfter } : {}),
    createdAt: now,
    updatedAt: now,
  });
}

////////////////////////////////////////////
// TRACK SEARCH
////////////////////////////////////////////

// Mirrors the searchable subset of the scoped-tracks orchestrator's Orama
// schema (which itself maps onto the Track definition).
const SEARCH_SCHEMA = {
  id: /** @type {const} */ ("string"),
  title: /** @type {const} */ ("string"),
  artist: /** @type {const} */ ("string"),
  album: /** @type {const} */ ("string"),
  genre: /** @type {const} */ ("string"),
  year: /** @type {const} */ ("number"),
};

/**
 * @param {Track} track
 */
const toSearchDoc = (track) => ({
  id: track.id,
  title: trackTitle(track),
  artist: [
    track?.tags?.artist,
    ...(track?.tags?.artists ?? []),
    track?.tags?.albumartist,
  ].filter(Boolean).join(" "),
  album: trackAlbum(track),
  genre: (track?.tags?.genres ?? []).join(" "),
  year: track?.tags?.year ?? 0,
});

/** @type {{ tracks: Track[]; db: any; Orama: any } | undefined} */
let searchCache;

/**
 * A lazily-built full-text index over the track collection. The collection's
 * array is stable while unchanged, so its identity doubles as the cache key.
 * Dynamically imported so the index cost is only paid when searching.
 *
 * @param {Track[]} tracks
 * @returns {Promise<{ tracks: Track[]; db: any; Orama: any }>}
 */
async function trackSearchIndex(tracks) {
  if (searchCache?.tracks === tracks) return searchCache;

  const Orama = await import("@orama/orama");
  const db = Orama.create({ schema: SEARCH_SCHEMA });
  await Orama.insertMultiple(db, tracks.map(toSearchDoc));

  searchCache = { tracks, db, Orama };
  return searchCache;
}

////////////////////////////////////////////
// PLAYBACK
////////////////////////////////////////////

/**
 * The queue and audio engines, created on first use (reused when the
 * interface facet already spun them up). Going through the queue-audio
 * orchestrator pulls in the full playback stack — the input configurator,
 * repeat & shuffle, and the glue that resolves the active queue item into
 * audio — so playback also works from pages that render no player themselves
 * (dashboard, editor, …).
 *
 * @returns {Promise<{
 *   queue: import("~/components/engine/queue/element.js").CLASS,
 *   audio: import("~/components/engine/audio/element.js").CLASS,
 *   repeatShuffle: import("~/components/engine/repeat-shuffle/element.js").CLASS,
 * }>} 
 */
async function engines() {
  await foundation.orchestrator.queueAudio();

  const [queue, audio, repeatShuffle] = await Promise.all([
    foundation.engine.queue(),
    foundation.engine.audio(),
    foundation.engine.repeatShuffle(),
  ]);

  await Promise.all([
    customElements.whenDefined(queue.localName),
    customElements.whenDefined(audio.localName),
    customElements.whenDefined(repeatShuffle.localName),
  ]);

  return { queue, audio, repeatShuffle };
}

/**
 * Waits for the queue-audio orchestrator to resolve & supply the given track —
 * the engine's `play()` silently no-ops on an item it doesn't know yet — then
 * starts playback. Reacts to the audio engine's item signal instead of
 * polling, and gives up after `timeoutMs` (e.g. on sources that never
 * resolve).
 *
 * @param {import("~/components/engine/audio/element.js").CLASS} audio
 * @param {string} audioId
 * @param {number} [timeoutMs]
 * @returns {Promise<boolean>} whether the track was supplied in time
 */
async function startPlayback(audio, audioId, timeoutMs = 15_000) {
  await new Promise((resolve) => {
    const timer = setTimeout(() => {
      stop();
      resolve(undefined);
    }, timeoutMs);

    const stop = effect(() => {
      if (audio.state(audioId)) {
        clearTimeout(timer);
        stop();
        resolve(undefined);
      }
    });
  });

  if (!audio.state(audioId)) return false;

  audio.play({ audioId });
  return true;
}

/**
 * Plays tracks immediately: the first added track becomes the active one, the
 * rest line up behind it. When audio is already playing, the queue-audio
 * orchestrator continues playback on its own.
 *
 * @param {import("~/components/engine/queue/element.js").CLASS} queue
 * @param {import("~/components/engine/audio/element.js").CLASS} audio
 * @param {string[]} trackIds
 */
async function playNow(queue, audio, trackIds) {
  const wasIdle = !queue.now();

  queue.add({ inFront: true, trackIds });
  // Adding to an idle queue already promotes the first added track.
  if (!wasIdle) queue.shift();

  if (audio.isPlaying()) return true;

  return await startPlayback(audio, trackIds[0]);
}

////////////////////////////////////////////
// TOOL REGISTRATION
////////////////////////////////////////////

/**
 * Every WebMCP host object: `document.modelContext` on current clients,
 * `navigator.modelContext` on older ones. Both can exist at the same time —
 * and be distinct registries — so the tools are registered on each of them.
 *
 * @returns {any[]}
 */
function getModelContexts() {
  /** @type {any[]} */
  const contexts = [];

  if ("modelContext" in document) {
    contexts.push(/** @type {any} */ (document).modelContext);
  }
  if ("modelContext" in navigator) {
    const modelContext = /** @type {any} */ (navigator).modelContext;
    if (!contexts.includes(modelContext)) contexts.push(modelContext);
  }

  return contexts;
}

/**
 * Waits briefly for a WebMCP host to appear. Chrome exposes the API from
 * document creation, but polyfills and extensions may inject it later.
 *
 * @param {number} [timeoutMs]
 * @param {AbortSignal} [signal]
 * @returns {Promise<any[]>}
 */
async function waitForModelContexts(timeoutMs = 2000, signal) {
  const step = 250;

  for (let waited = 0; waited <= timeoutMs; waited += step) {
    if (signal?.aborted) return [];

    const contexts = getModelContexts().filter((c) => c?.registerTool);
    if (contexts.length) return contexts;
    await new Promise((resolve) => setTimeout(resolve, step));
  }

  return [];
}

/**
 * Registers the Diffuse tools with the browser's agent via WebMCP. Tools only
 * reach external agents (e.g. over Chrome's CDP `WebMCP` domain) when they're
 * in the browser's registry, so every registration failure is logged loudly.
 *
 * Abort the given signal to unregister the tools again — supported by the
 * spec's `registerTool` options, and a no-op where it isn't.
 *
 * @param {{ signal?: AbortSignal }} [options]
 */
export async function registerDiffuseTools(options = {}) {
  const signal = options.signal;
  const contexts = await waitForModelContexts(2000, signal);
  if (!contexts.length) {
    if (signal?.aborted) return;

    const present = getModelContexts();
    if (present.length) {
      console.error(
        "WebMCP: a model context exists but exposes no registerTool() — the " +
          "Diffuse tools were NOT registered. Check the browser's WebMCP " +
          "tools permission or chrome://flags experiments.",
      );
    } else {
      // An unsupported browser is a normal situation, not an error.
      console.info(
        "WebMCP: not supported in this browser — the Diffuse tools were not registered.",
      );
    }
    return;
  }

  let registered = 0;
  let failed = 0;

  /**
   * @param {{
   *   name: string,
   *   title?: string,
   *   description: string,
   *   inputSchema: Record<string, any>,
   *   annotations?: Partial<{
   *     readOnlyHint: boolean,
   *     untrustedContentHint: boolean,
   *     consequentialHint: boolean,
   *     debugging: boolean,
   *   }>,
   *   execute: (args: Record<string, any>) => Promise<unknown>,
   * }} tool
   */
  const register = async (tool) => {
    for (const modelContext of contexts) {
      if (signal?.aborted) return;

      try {
        await modelContext.registerTool(
          tool,
          signal ? { signal } : undefined,
        );
        registered++;
      } catch (e) {
        failed++;
        console.error(`WebMCP: failed to register "${tool.name}"`, e);
        if (/** @type {any} */ (e)?.name === "NotAllowedError") {
          console.error(
            "WebMCP: tool registration was not allowed — check the browser's " +
              "WebMCP/tools permission or chrome://flags experiments.",
          );
        }
      }
    }
  };

  // — DOCS ———————————————————————————————————————————

  await register({
    name: "get-facet-docs",
    annotations: { readOnlyHint: true },
    description:
      "Reads the Diffuse development documentation — the same context the " +
      "diffuse-facet agent skill provides: how to write facets, every available " +
      "component/element, the foundation API and the data schemas. Call without "+
      "'path' to list the available documents. Read SKILL.md before creating or "+
      "editing facets.",
    inputSchema: {
      type: "object",
      properties: {
        path: {
          type: "string",
          description:
            "Document path from the index. Paths under docs/definitions/ (lexicons and generated types) also resolve.",
        },
        query: {
          type: "string",
          description:
            "For docs/elements.txt: only return elements whose name or source matches this substring.",
        },
      },
    },
    async execute({ path: docPath, query }) {
      if (!docPath) {
        return ok(JSON.stringify({ docs: DOCS }, null, 2));
      }

      if (docPath.includes("..")) {
        return ok(`Invalid document path "${docPath}".`);
      }

      const doc = docPath.replace(/^\/+/, "");

      if (doc === "docs/elements.txt") {
        const text = await fetchDoc(doc);
        if (text === undefined) {
          return ok(`Could not read "${docPath}". Call without 'path' to list the available documents.`);
        }
        return ok(elementsDoc(text, query));
      }

      const text = await fetchDoc(doc);
      if (text === undefined) {
        return ok(`Could not read "${docPath}". Call without 'path' to list the available documents.`);
      }

      return ok(text);
    },
  });

  // — FACETS —————————————————————————————————————————

  await register({
    name: "list-facets",
    annotations: { readOnlyHint: true },
    description:
      "Lists the facets in the user's Diffuse collection — interfaces (themes, " +
      "browsers, players) and preludes (headless features that run on every " +
      "facet). Returns id, name, kind, description, enabled, favourite, tags and " +
      "where the facet's code lives (custom inline code, the Diffuse bundle, or " +
      "an external URI) — but not the code itself (use get-facet for that). " +
      "Hidden 'base' facets are excluded by default.",
    inputSchema: {
      type: "object",
      properties: {
        includeBase: {
          type: "boolean",
          description: "Also include hidden essential ('base') facets.",
          default: false,
        },
        kind: {
          type: "string",
          enum: ["interactive", "prelude"],
          description: "Only return facets of this kind.",
        },
      },
    },
    async execute({ includeBase, kind }) {
      let facets = await listFacets();

      if (!includeBase) {
        facets = facets.filter((f) => !f.tags?.includes("base"));
      }
      if (kind) facets = facets.filter((f) => (f.kind ?? "interactive") === kind);

      const summary = facets.map(facetSummary);
      return ok(JSON.stringify({ total: summary.length, facets: summary }, null, 2));
    },
  });

  await register({
    name: "get-facet",
    annotations: { readOnlyHint: true, untrustedContentHint: true },
    description:
      "Reads a single facet: its metadata plus its full source code. Single-file " +
      "facets return 'html'; facets that ship extra files (modules, stylesheets) " +
      "also return 'files', a list of { path, content }. Use this to read a facet " +
      "before improving its code. Facet code can be large and may exceed client " +
      "result limits — prefer list-facets first to find the right one and " +
      "get-facet-file to read large facets one file at a time.",
    inputSchema: {
      type: "object",
      properties: {
        id: { type: "string", description: "Facet id from list-facets." },
      },
      required: ["id"],
    },
    async execute({ id }) {
      const facets = await listFacets();
      const facet = facets.find((f) => f.id === id);
      if (!facet) {
        return ok(`No facet with id "${id}" was found.`);
      }

      const files = await facetFiles(facet);
      if (!files) {
        return ok(
          `Facet "${facet.name}" (${facet.uri}) content could not be resolved.`,
        );
      }

      const html = files.find((f) => f.path === "/")?.content ?? "";
      return ok(JSON.stringify({
        ...facetSummary(facet),
        html,
        ...(files.length > 1 ? { files } : {}),
      }, null, 2));
    },
  });

  await register({
    name: "get-facet-file",
    annotations: { readOnlyHint: true, untrustedContentHint: true },
    description:
      "Reads ONE file of a facet, by id and path — facet code can exceed client " +
      "result limits when returned whole, so read large facets file by file. " +
      "Call with only 'id' to list the facet's files and their sizes.",
    inputSchema: {
      type: "object",
      properties: {
        id: { type: "string", description: "Facet id from list-facets." },
        path: {
          type: "string",
          description: "File path from the facet's file list, e.g. '/' or '/facet.js'. Omit to list the files.",
        },
      },
      required: ["id"],
    },
    async execute({ id, path }) {
      const facets = await listFacets();
      const facet = facets.find((f) => f.id === id);
      if (!facet) {
        return ok(`No facet with id "${id}" was found.`);
      }

      const files = await facetFiles(facet);
      if (!files?.length) {
        return ok(`Facet "${facet.name}" (${facet.uri}) content could not be resolved.`);
      }

      if (path === undefined) {
        return ok(JSON.stringify({
          name: facet.name,
          files: files.map((f) => ({ path: f.path, characters: f.content.length })),
        }, null, 2));
      }

      const normalized = "/" + path.replace(/^\/+/, "");
      const file = files.find((f) => f.path === normalized);
      if (!file) {
        return ok(
          `No file "${path}" in "${facet.name}". Available: ` +
            files.map((f) => f.path).join(", "),
        );
      }

      return ok(file.content);
    },
  });

  await register({
    name: "create-facet",
    description:
      "Creates a new facet and saves it to the user's collection. Read the " +
      "development docs via get-facet-docs (start with SKILL.md) before writing " +
      "the code. Pass the complete facet code either as 'html' (one document, no " +
      "<!doctype>/<html>/<head>; the loader wraps it) or as 'files' (a map of " +
      "absolute path to content, which must include '/'). kind: 'interactive' " +
      "for an interface, 'prelude' for a headless feature that runs on every facet.",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Facet name." },
        kind: {
          type: "string",
          enum: ["interactive", "prelude"],
          description: "interactive = interface, prelude = feature.",
          default: "interactive",
        },
        description: { type: "string", description: "Short description." },
        html: { type: "string", description: "Complete facet HTML source." },
        files: {
          type: "object",
          additionalProperties: { type: "string" },
          description:
            "Complete facet source as a map of absolute path to content, e.g. " +
            `{ "/": "…", "/style.css": "…" }. Must include '/'.`,
        },
        tags: { type: "array", items: { type: "string" }, description: "Optional tags." },
      },
      required: ["name"],
    },
    async execute({ name, kind, description, html, files, tags }) {
      if (html === undefined && files === undefined) {
        return ok("Provide 'html' or 'files' with the facet's source code.");
      }
      if (files !== undefined && !files["/"]) {
        return ok("The 'files' map must include the '/' index document.");
      }

      const now = new Date().toISOString();
      const tile = files !== undefined
        ? tileFromFiles(files)
        : htmlFacetTile(/** @type {string} */ (html));

      /** @type {Facet} */
      const facet = {
        $type: FACET_TYPE,
        id: TID.now(),
        name,
        kind: kind === "prelude" ? "prelude" : "interactive",
        description: description ?? undefined,
        blocks: tile.blocks,
        resources: tile.resources,
        tags: tags?.length ? tags : undefined,
        createdAt: now,
        updatedAt: now,
      };

      await saveFacet(facet);

      return ok(JSON.stringify({
        created: true,
        id: facet.id,
        name: facet.name,
        ...(facet.kind === "prelude"
          ? {
            note:
              "Preludes activate on the next facet load; the page you're on " +
              "reloads by itself. If it doesn't, open any facet again.",
          }
          : {}),
      }, null, 2));
    },
  });

  await register({
    name: "update-facet",
    description:
      "Updates an existing facet by id. Omitted fields are left unchanged. " +
      "Read the facet's current code with get-facet, and the development docs " +
      "via get-facet-docs, before editing. 'html' replaces the facet's index " +
      "document while keeping its other files; 'files' replaces the facet's " +
      "complete source (must include '/'). Updating a facet that tracks the " +
      "Diffuse bundle or an external URI saves your edited copy with the facet " +
      "(its code becomes custom code). Enabling or disabling a prelude takes " +
      "effect when the page reloads by itself.",
    inputSchema: {
      type: "object",
      properties: {
        id: { type: "string", description: "Facet id from list-facets." },
        name: { type: "string" },
        kind: { type: "string", enum: ["interactive", "prelude"] },
        description: { type: "string" },
        enabled: { type: "boolean" },
        favourite: { type: "boolean" },
        tags: { type: "array", items: { type: "string" } },
        html: {
          type: "string",
          description: "New index document; the facet's other files are kept.",
        },
        files: {
          type: "object",
          additionalProperties: { type: "string" },
          description:
            "Complete replacement source, keyed by absolute path. Must include '/'.",
        },
      },
      required: ["id"],
    },
    async execute({ id, name, kind, description, enabled, favourite, tags, html, files }) {
      const facets = await listFacets();
      const existing = facets.find((f) => f.id === id);
      if (!existing) return ok(`No facet with id "${id}" was found.`);

      /** @type {Facet} */
      const facet = { ...existing };

      if (name !== undefined) facet.name = name;
      if (kind !== undefined) facet.kind = kind === "prelude" ? "prelude" : "interactive";
      if (description !== undefined) facet.description = description;
      if (enabled !== undefined) facet.enabled = enabled;
      if (favourite !== undefined) facet.favourite = favourite;
      if (tags !== undefined) facet.tags = tags.length ? tags : undefined;

      if (html !== undefined || files !== undefined) {
        // Editing a bundled facet forks it into custom code. Drop the bundle
        // uri: the legacy-facet migration treats an inline root-only tile on a
        // `diffuse://` facet as a stale frozen copy and strips it in favour of
        // the live bundle, which would silently discard this edit.
        if (facet.uri?.startsWith("diffuse://")) {
          delete facet.uri;
        }

        if (files !== undefined) {
          if (!files["/"]) {
            return ok("The 'files' map must include the '/' index document.");
          }

          const tile = tileFromFiles(files);
          facet.resources = tile.resources;
          facet.blocks = tile.blocks;
        } else {
          // Replace the index document but keep the facet's other tile files,
          // so absolute references (/facet.js, /style.css, …) keep working.
          const existingFiles = (await facetFiles(existing)) ?? [];
          const others = existingFiles.filter((f) => f.path !== "/");
          const tile = others.length
            ? tileFromFiles({
              ...Object.fromEntries(others.map((f) => [f.path, f.content])),
              "/": /** @type {string} */ (html),
            })
            : htmlFacetTile(/** @type {string} */ (html));

          facet.resources = tile.resources;
          facet.blocks = tile.blocks;
        }
      }

      await saveFacet(facet); // bumps updatedAt

      return ok(JSON.stringify({
        updated: true,
        id: facet.id,
        name: facet.name,
      }, null, 2));
    },
  });

  await register({
    name: "set-facet-enabled",
    description:
      "Enables or disables a facet by id. Disabling an interface dims it on the " +
      "dashboard; disabling a prelude stops the feature from running on any " +
      "facet — e.g. disable the 'Automatic Queue' prelude to stop automatic " +
      "queue additions. The page reloads by itself to apply the change.",
    inputSchema: {
      type: "object",
      properties: {
        id: { type: "string", description: "Facet id from list-facets." },
        enabled: { type: "boolean", description: "True to enable, false to disable." },
      },
      required: ["id", "enabled"],
    },
    async execute({ id, enabled }) {
      const facets = await listFacets();
      const facet = facets.find((f) => f.id === id);
      if (!facet) return ok(`No facet with id "${id}" was found.`);

      if ((facet.enabled ?? true) === enabled) {
        return ok(`"${facet.name}" is already ${enabled ? "enabled" : "disabled"}.`);
      }

      await saveFacet({ ...facet, enabled });

      return ok(
        facet.kind === "prelude"
          ? `${enabled ? "Enabled" : "Disabled"} prelude "${facet.name}". The page reloads by itself to apply it.`
          : `${enabled ? "Enabled" : "Disabled"} facet "${facet.name}".`,
      );
    },
  });

  await register({
    name: "delete-facet",
    annotations: { consequentialHint: true },
    description:
      "Deletes a facet from the user's collection by id. Facets that are part " +
      "of the Diffuse bundle can be re-added from the dashboard afterwards; " +
      "deleted custom facets are gone for good.",
    inputSchema: {
      type: "object",
      properties: {
        id: { type: "string", description: "Facet id from list-facets." },
      },
      required: ["id"],
    },
    async execute({ id }) {
      const out = await foundation.orchestrator.output();
      const col = await Output.data(out.facets);
      const facet = col.find((f) => f.id === id);
      if (!facet) return ok(`No facet with id "${id}" was found.`);

      await out.facets.save(col.filter((c) => c.id !== id));
      return ok(`Deleted facet "${facet.name}" (${id}).`);
    },
  });

  // — PLAYBACK ————————————————————————————————————————

  await register({
    name: "list-playlists",
    annotations: { readOnlyHint: true },
    description:
      "Lists the user's playlists with their track counts. Playlists are " +
      "criteria-based: 'ordered' means the playlist defines a manual track " +
      "order; 'ephemeral' means it is generated on the fly (e.g. from folder " +
      "structure) and not stored.",
    inputSchema: { type: "object", properties: {} },
    async execute() {
      const [items, tracks] = await Promise.all([
        collectionPlaylistItems(),
        collectionTracks(),
      ]);

      const playlists = [...gather(items).values()]
        .map((group) => ({
          name: group.name,
          tracks: filterByPlaylist(tracks, group.items).length,
          ordered: !group.unordered,
          ephemeral: group.items.every((i) => i.ephemeral ?? false),
        }))
        .sort((a, b) => a.name.localeCompare(b.name));

      return ok(JSON.stringify({ total: playlists.length, playlists }, null, 2));
    },
  });

  await register({
    name: "get-playlist",
    annotations: { readOnlyHint: true },
    description:
      "Returns a playlist's tracks with the ids needed by queue-tracks / " +
      "play-tracks. Ordered playlists return their tracks in playlist order; " +
      "unordered ones are sorted by artist, album and title.",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Playlist name from list-playlists." },
        limit: {
          type: "integer",
          description: "Maximum number of tracks to return (default 200, max 500).",
          default: 200,
        },
      },
      required: ["name"],
    },
    async execute({ name, limit }) {
      const [items, tracks] = await Promise.all([
        collectionPlaylistItems(),
        collectionTracks(),
      ]);

      let playlistItems = items.filter((i) => i.playlist === name);
      if (!playlistItems.length) {
        const lower = name.toLowerCase();
        playlistItems = items.filter((i) => i.playlist.toLowerCase() === lower);
      }
      if (!playlistItems.length) {
        return ok(`No playlist named "${name}" was found.`);
      }

      const unordered = playlistItems.some((i) => i.positionedAfter == null);
      const matched = filterByPlaylist(tracks, playlistItems);
      const ordered = unordered
        ? sortTracks(matched)
        : orderByPlaylist(matched, playlistItems);

      const max = Math.min(Math.max(limit ?? 200, 1), 500);
      return ok(JSON.stringify({
        name,
        total: ordered.length,
        showing: Math.min(ordered.length, max),
        tracks: ordered.slice(0, max).map(trackSummary),
      }, null, 2));
    },
  });

  await register({
    name: "search-tracks",
    annotations: { readOnlyHint: true },
    description:
      "Full-text search over the user's track collection (title, artist, album, " +
      "genre, year — every term must match, ranked by relevance). Returns " +
      "matching tracks with the ids needed by queue-tracks / play-tracks. " +
      "Omit 'query' to sample the collection instead.",
    inputSchema: {
      type: "object",
      properties: {
        query: { type: "string", description: "Search query, e.g. an artist or song title." },
        limit: {
          type: "integer",
          description: "Maximum number of tracks to return (default 25, max 100).",
          default: 25,
        },
      },
    },
    async execute({ query, limit }) {
      const tracks = await collectionTracks();
      const q = (query ?? "").trim();
      const max = Math.min(Math.max(limit ?? 25, 1), 100);

      /** @type {Track[]} */
      let matches;
      let total;

      if (q) {
        try {
          const { db, Orama } = await trackSearchIndex(tracks);

          /** @type {{ hits: { document: { id: string } }[]; count: number }} */
          const results = await Orama.search(db, {
            term: q,
            threshold: 0, // AND: every term must match at least one field
            limit: max,
          });

          const byId = new Map(tracks.map((t) => [t.id, t]));
          matches = results.hits
            .map((hit) => byId.get(hit.document.id))
            .filter((t) => t !== undefined);
          total = results.count;
        } catch (e) {
          console.warn("WebMCP: track search index unavailable, falling back to substring matching", e);

          const needle = q.toLowerCase();
          matches = tracks.filter((t) => trackHaystack(t).includes(needle));
          total = matches.length;
        }
      } else {
        matches = sortTracks(tracks);
        total = tracks.length;
      }

      return ok(JSON.stringify({
        total,
        showing: Math.min(matches.length, max),
        tracks: matches.slice(0, max).map(trackSummary),
      }, null, 2));
    },
  });

  await register({
    name: "queue-tracks",
    description:
      "Adds tracks to the playback queue by id. 'inFront' puts them at the front " +
      "of the queue (up next); otherwise they are added at the back, behind the " +
      "tracks already queued manually. When nothing is queued the first track " +
      "becomes the active track. Use search-tracks or get-playlist to find ids.",
    inputSchema: {
      type: "object",
      properties: {
        trackIds: {
          type: "array",
          items: { type: "string" },
          description: "Track ids, in the order they should play.",
        },
        inFront: {
          type: "boolean",
          description: "Add to the front of the queue instead of the back.",
          default: false,
        },
      },
      required: ["trackIds"],
    },
    async execute({ trackIds, inFront }) {
      if (!trackIds?.length) return ok("Provide at least one track id.");

      const tracks = await collectionTracks();
      const unknown = unknownTrackIds(tracks, trackIds);
      if (unknown.length) {
        return ok(`No track(s) with id(s): ${unknown.join(", ")}. Use search-tracks to find ids.`);
      }

      const { queue } = await engines();
      const wasIdle = !queue.now();

      queue.add({ inFront: !!inFront, trackIds });

      return ok(JSON.stringify({
        added: trackIds.length,
        position: inFront ? "front of the queue (up next)" : "back of the queue",
        startedQueue: wasIdle,
      }, null, 2));
    },
  });

  await register({
    name: "play-tracks",
    description:
      "Plays tracks immediately: the first track starts now, the rest line up " +
      "behind it. Whatever is currently playing moves to the queue's history. " +
      "Use search-tracks or get-playlist to find ids.",
    inputSchema: {
      type: "object",
      properties: {
        trackIds: {
          type: "array",
          items: { type: "string" },
          description: "Track ids, in the order they should play.",
        },
      },
      required: ["trackIds"],
    },
    async execute({ trackIds }) {
      if (!trackIds?.length) return ok("Provide at least one track id.");

      const tracks = await collectionTracks();
      const unknown = unknownTrackIds(tracks, trackIds);
      if (unknown.length) {
        return ok(`No track(s) with id(s): ${unknown.join(", ")}. Use search-tracks to find ids.`);
      }

      const { queue, audio } = await engines();
      const started = await playNow(queue, audio, trackIds);

      return ok(JSON.stringify({
        playing: trackIds[0],
        started,
        queued: trackIds.length,
      }, null, 2));
    },
  });

  await register({
    name: "get-playback-state",
    annotations: { readOnlyHint: true },
    description:
      "Returns the current playback state: what's playing (with position), the " +
      "upcoming queue (with the keys used by remove-from-queue / move-in-queue), " +
      "recent history, volume, repeat and shuffle. Audio positions are in " +
      "seconds; track durations in milliseconds.",
    inputSchema: { type: "object", properties: {} },
    async execute() {
      const { queue, audio, repeatShuffle } = await engines();
      const tracks = await collectionTracks();
      const byId = new Map(tracks.map((t) => [t.id, t]));

      const now = queue.now();
      const state = now ? audio.state(now.id) : undefined;
      const past = queue.past();
      const future = queue.future();

      /**
       * @param {{ id: string; key: string; manualEntry: boolean }} item
       */
      const itemSummary = (item) => ({
        key: item.key,
        manual: item.manualEntry,
        track: byId.get(item.id) ? trackSummary(byId.get(item.id)) : { id: item.id },
      });

      return ok(JSON.stringify({
        playing: audio.isPlaying(),
        volume: audio.volume(),
        repeat: repeatShuffle.repeat(),
        shuffle: repeatShuffle.shuffle(),
        now: now
          ? {
            ...itemSummary(now),
            // Seconds, from the audio engine.
            currentTime: state ? state.currentTime() : null,
            duration: state ? state.duration() : null,
            progress: state ? state.progress() : null,
          }
          : null,
        // Most recent first.
        history: past.slice(-10).reverse().map(itemSummary),
        historyTotal: past.length,
        queue: future.slice(0, 50).map(itemSummary),
        queueTotal: future.length,
      }, null, 2));
    },
  });

  await register({
    name: "control-playback",
    description:
      "Transport controls: pause, resume, skip to the next track, go back to " +
      "the previous one, seek within the current track, or set the volume. " +
      "Resuming an idle queue starts it with its first upcoming track.",
    inputSchema: {
      type: "object",
      properties: {
        action: {
          type: "string",
          enum: ["pause", "resume", "next", "previous", "seek", "set-volume"],
          description: "The transport action to perform.",
        },
        percentage: {
          type: "number",
          description: "For 'seek': position in the track, 0–1.",
        },
        volume: {
          type: "number",
          description: "For 'set-volume': 0–1.",
        },
      },
      required: ["action"],
    },
    async execute({ action, percentage, volume }) {
      const { queue, audio } = await engines();
      const now = queue.now();

      switch (action) {
        case "pause": {
          if (!now) return ok("Nothing is playing.");
          audio.pause({ audioId: now.id });
          return ok("Paused.");
        }

        case "resume": {
          if (now) {
            audio.play({ audioId: now.id });
            return ok("Resumed.");
          }

          // An idle queue: start it with its first upcoming track.
          const future = queue.future();
          if (!future.length) return ok("Nothing is queued to resume.");

          queue.shift();
          await startPlayback(audio, future[0].id);
          return ok("Started the queue.");
        }

        case "next": {
          if (!now && !queue.future().length) return ok("Nothing is queued.");
          queue.shift();
          return ok("Skipped to the next track.");
        }

        case "previous": {
          if (!now && !queue.past().length) {
            return ok("Nothing is playing and there is no history.");
          }
          queue.unshift();
          return ok("Went back to the previous track.");
        }

        case "seek": {
          if (!now) return ok("Nothing is playing.");
          if (
            typeof percentage !== "number" || percentage < 0 || percentage > 1
          ) {
            return ok("Provide a 'percentage' between 0 and 1.");
          }
          audio.seek({ audioId: now.id, percentage });
          return ok(`Seeked to ${Math.round(percentage * 100)}%.`);
        }

        case "set-volume": {
          if (
            typeof volume !== "number" || volume < 0 || volume > 1
          ) {
            return ok("Provide a 'volume' between 0 and 1.");
          }
          audio.adjustVolume({ volume });
          return ok(`Volume set to ${Math.round(volume * 100)}%.`);
        }

        default:
          return ok(`Unknown action "${action}".`);
      }
    },
  });

  await register({
    name: "set-repeat-shuffle",
    description:
      "Turns repeat and/or shuffle on or off. Turning shuffle on or off clears " +
      "the automatically-filled part of the queue and refills it.",
    inputSchema: {
      type: "object",
      properties: {
        shuffle: { type: "boolean", description: "Shuffle the automatic queue filling." },
        repeat: { type: "boolean", description: "Repeat the current track when it ends." },
      },
    },
    async execute({ shuffle, repeat }) {
      if (shuffle === undefined && repeat === undefined) {
        return ok("Provide 'shuffle' and/or 'repeat'.");
      }

      const { repeatShuffle } = await engines();
      if (shuffle !== undefined) await repeatShuffle.setShuffle(shuffle);
      if (repeat !== undefined) await repeatShuffle.setRepeat(repeat);

      return ok(JSON.stringify({
        shuffle: repeatShuffle.shuffle(),
        repeat: repeatShuffle.repeat(),
      }, null, 2));
    },
  });

  await register({
    name: "clear-queue",
    description:
      "Removes the upcoming tracks from the queue. What's currently playing and " +
      "the history are kept. With 'keepManual', automatically-filled tracks are " +
      "cleared but manually queued ones are kept.",
    inputSchema: {
      type: "object",
      properties: {
        keepManual: {
          type: "boolean",
          description: "Keep manually queued tracks (only clear automatic ones).",
          default: false,
        },
      },
    },
    async execute({ keepManual }) {
      const { queue } = await engines();
      const count = queue.future().length;

      queue.clear({ keepManual: !!keepManual });

      const remaining = queue.future().length;
      return ok(
        count === 0
          ? "The queue was already empty."
          : `Removed ${count - remaining} of ${count} upcoming track(s); ${remaining} left.`,
      );
    },
  });

  await register({
    name: "remove-from-queue",
    description:
      "Removes one track from the queue (upcoming or history) by its key, as " +
      "returned by get-playback-state. Cannot remove the track that's currently " +
      "playing.",
    inputSchema: {
      type: "object",
      properties: {
        key: { type: "string", description: "Queue item key from get-playback-state." },
      },
      required: ["key"],
    },
    async execute({ key }) {
      const { queue } = await engines();

      const known = queue.future().some((i) => i.key === key) ||
        queue.past().some((i) => i.key === key);
      if (!known) {
        return ok(`No queue item with key "${key}" — use get-playback-state for the current keys.`);
      }

      queue.expel({ key });
      return ok("Removed the track from the queue.");
    },
  });

  await register({
    name: "move-in-queue",
    description:
      "Reorders an upcoming queue item. 'position' is the index within the " +
      "upcoming queue after the move — 0 makes the track play next.",
    inputSchema: {
      type: "object",
      properties: {
        key: { type: "string", description: "Queue item key from get-playback-state." },
        position: {
          type: "integer",
          description: "Index within the upcoming queue, 0 = play next.",
        },
      },
      required: ["key", "position"],
    },
    async execute({ key, position }) {
      const { queue } = await engines();
      const future = queue.future();

      const index = future.findIndex((i) => i.key === key);
      if (index < 0) {
        return ok(`No upcoming queue item with key "${key}" — use get-playback-state for the current keys.`);
      }
      if (typeof position !== "number" || position < 0) {
        return ok("Provide a 'position' of 0 or higher (0 = play next).");
      }

      // `move` operates on the flat [history…, now, upcoming…] list.
      const to = queue.past().length + (queue.now() ? 1 : 0) +
        Math.min(Math.round(position), future.length - 1);
      queue.move({ key, to });

      return ok(`Moved the track to position ${Math.min(Math.round(position), future.length - 1)} of the upcoming queue.`);
    },
  });

  // — PLAYLISTS & FAVOURITES ———————————————————

  await register({
    name: "set-favourite",
    description:
      "Adds tracks to, removes them from, or toggles the user's favourites. " +
      "With 'favourited' omitted, tracks already in favourites are removed and " +
      "the rest are added. The favourites live in a playlist named " +
      "\"Favourites\" — list-playlists / get-playlist can read them back.",
    inputSchema: {
      type: "object",
      properties: {
        trackIds: {
          type: "array",
          items: { type: "string" },
          description: "Track ids from search-tracks / get-playlist.",
        },
        favourited: {
          type: "boolean",
          description: "True to add to favourites, false to remove. Omit to toggle each track.",
        },
      },
      required: ["trackIds"],
    },
    async execute({ trackIds, favourited }) {
      if (!trackIds?.length) return ok("Provide at least one track id.");

      const { tracks, unknown } = await resolveTrackIds(trackIds);
      if (unknown.length) {
        return ok(`No track(s) with id(s): ${unknown.join(", ")}. Use search-tracks to find ids.`);
      }

      const favourites = await foundation.orchestrator.favourites();

      if (favourited === true) await favourites.include(tracks);
      else if (favourited === false) await favourites.expel(tracks);
      else await favourites.toggle(tracks);

      return ok(
        favourited === false
          ? `Removed ${tracks.length} track(s) from favourites.`
          : `Favourited ${tracks.length} track(s).`,
      );
    },
  });

  await register({
    name: "create-playlist",
    description:
      "Creates a playlist, in one of three ways: with 'trackIds' (a playlist " +
      "containing exactly those tracks, in order when 'ordered'), with " +
      "'criteria' (a rule-based playlist that automatically matches tracks — " +
      "e.g. field 'tags.artist' value 'Radiohead'), or with neither (an empty " +
      "placeholder playlist). Rule-based playlists match tracks by exact value " +
      "equality; they cannot do free-text or range matching.",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Playlist name." },
        trackIds: {
          type: "array",
          items: { type: "string" },
          description: "Track ids from search-tracks, in the order they should appear.",
        },
        ordered: {
          type: "boolean",
          description: "Keep the given track order (default true; ignored with 'criteria').",
          default: true,
        },
        criteria: {
          type: "array",
          description: "Rule-based playlist: every criterion must match a track for it to belong.",
          items: {
            type: "object",
            properties: {
              field: {
                type: "string",
                description:
                  "Track field path, e.g. tags.artist, tags.title, tags.album, tags.albumartist, tags.year, id, uri.",
              },
              value: { description: "Exact value the field must equal." },
              transformations: {
                type: "array",
                items: { type: "string" },
                description:
                  "Applied to both values before comparing, e.g. [\"toLowerCase\"].",
              },
            },
            required: ["field", "value"],
          },
        },
      },
      required: ["name"],
    },
    async execute({ name, trackIds, criteria, ordered = true }) {
      const playlist = (name ?? "").trim();
      if (!playlist) return ok("Provide a playlist 'name'.");

      const out = await foundation.orchestrator.output();
      const items = await Output.data(out.playlistItems);

      if (findPlaylistItems(items, playlist).length) {
        return ok(`A playlist named "${playlist}" already exists.`);
      }

      const now = new Date().toISOString();

      /** @type {PlaylistItem[]} */
      const created = [];

      if (criteria?.length) {
        if (
          criteria.some((/** @type {any} */ c) => !c || typeof c.field !== "string" || !("value" in c))
        ) {
          return ok("Each criterion needs a 'field' (e.g. 'tags.artist') and a 'value'.");
        }

        created.push({
          $type: PLAYLIST_ITEM_TYPE,
          id: TID.now(),
          playlist,
          criteria,
          createdAt: now,
          updatedAt: now,
        });
      } else if (trackIds?.length) {
        const { tracks, unknown } = await resolveTrackIds(trackIds);
        if (unknown.length) {
          return ok(`No track(s) with id(s): ${unknown.join(", ")}. Use search-tracks to find ids.`);
        }

        tracks.forEach((track, i) => {
          created.push(
            trackPlaylistItem(
              playlist,
              track,
              ordered ? created[created.length - 1]?.id : undefined,
            ),
          );
        });
      } else {
        // Anchor item so the empty playlist exists — same as the Playlists facet.
        created.push({
          $type: PLAYLIST_ITEM_TYPE,
          id: TID.now(),
          playlist,
          criteria: [],
          createdAt: now,
          updatedAt: now,
        });
      }

      await out.playlistItems.save([...items, ...created]);

      return ok(JSON.stringify({
        created: playlist,
        items: created.length,
        kind: criteria?.length ? "rule-based" : trackIds?.length ? "track list" : "empty",
      }, null, 2));
    },
  });

  await register({
    name: "add-to-playlist",
    description:
      "Appends tracks to an existing playlist. Tracks already in the playlist " +
      "are skipped. Ordered playlists keep their order: the new tracks line up " +
      "at the end.",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Playlist name from list-playlists." },
        trackIds: {
          type: "array",
          items: { type: "string" },
          description: "Track ids from search-tracks / get-playlist.",
        },
      },
      required: ["name", "trackIds"],
    },
    async execute({ name, trackIds }) {
      if (!trackIds?.length) return ok("Provide at least one track id.");

      const out = await foundation.orchestrator.output();
      const items = await Output.data(out.playlistItems);
      const playlistItems = findPlaylistItems(items, name);
      if (!playlistItems.length) {
        return ok(`No playlist named "${name}" was found.`);
      }

      const { tracks, unknown } = await resolveTrackIds(trackIds);
      if (unknown.length) {
        return ok(`No track(s) with id(s): ${unknown.join(", ")}. Use search-tracks to find ids.`);
      }

      // Skip tracks the playlist already contains.
      const newTracks = tracks.filter((track) =>
        !playlistItems.some((item) => matchPlaylistItem(track, item))
      );
      if (!newTracks.length) {
        return ok(`All ${tracks.length} track(s) are already in "${name}".`);
      }

      const ordered = gather(playlistItems).get(playlistItems[0].playlist)
        ?.unordered === false;

      /** @type {string | undefined} */
      let positionedAfter = undefined;
      if (ordered) {
        const chain = sortPlaylistItems(playlistItems);
        positionedAfter = chain[chain.length - 1]?.id;
      }

      const now = new Date().toISOString();
      const created = newTracks.map((track) => {
        const item = trackPlaylistItem(playlistItems[0].playlist, track, positionedAfter);
        positionedAfter = ordered ? item.id : undefined;
        return item;
      });

      await out.playlistItems.save([...items, ...created]);

      return ok(
        `Added ${created.length} of ${tracks.length} track(s) to "${playlistItems[0].playlist}"` +
          `${tracks.length - created.length ? ` (${tracks.length - created.length} already in it)` : ""}.`,
      );
    },
  });

  await register({
    name: "remove-from-playlist",
    description: "Removes tracks from an existing playlist, by track id.",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Playlist name from list-playlists." },
        trackIds: {
          type: "array",
          items: { type: "string" },
          description: "Track ids to remove.",
        },
      },
      required: ["name", "trackIds"],
    },
    async execute({ name, trackIds }) {
      if (!trackIds?.length) return ok("Provide at least one track id.");

      const out = await foundation.orchestrator.output();
      const items = await Output.data(out.playlistItems);
      const playlistItems = findPlaylistItems(items, name);
      if (!playlistItems.length) {
        return ok(`No playlist named "${name}" was found.`);
      }

      const { tracks, unknown } = await resolveTrackIds(trackIds);
      if (unknown.length) {
        return ok(`No track(s) with id(s): ${unknown.join(", ")}. Use search-tracks to find ids.`);
      }

      const removedIds = new Set(
        playlistItems
          .filter((item) =>
            tracks.some((track) => matchPlaylistItem(track, item))
          )
          .map((item) => item.id),
      );

      if (!removedIds.size) {
        return ok(`None of the tracks are in "${playlistItems[0].playlist}".`);
      }

      await out.playlistItems.save(
        items.filter((item) => !removedIds.has(item.id)),
      );

      return ok(`Removed ${removedIds.size} item(s) from "${playlistItems[0].playlist}".`);
    },
  });

  await register({
    name: "delete-playlist",
    annotations: { consequentialHint: true },
    description:
      "Deletes a playlist by removing all of its items. The playlist's tracks " +
      "in the library are not affected. This cannot be undone.",
    inputSchema: {
      type: "object",
      properties: {
        name: { type: "string", description: "Playlist name from list-playlists." },
      },
      required: ["name"],
    },
    async execute({ name }) {
      const out = await foundation.orchestrator.output();
      const items = await Output.data(out.playlistItems);
      const playlistItems = findPlaylistItems(items, name);
      if (!playlistItems.length) {
        return ok(`No playlist named "${name}" was found.`);
      }

      const removedNames = new Set(playlistItems.map((i) => i.playlist));
      await out.playlistItems.save(
        items.filter((item) => !removedNames.has(item.playlist)),
      );

      return ok(`Deleted playlist "${[...removedNames][0]}" (${playlistItems.length} item(s)).`);
    },
  });

  // — LIBRARY ————————————————————————————————————————

  await register({
    name: "get-diffuse-state",
    annotations: { readOnlyHint: true },
    description:
      "Orients you in the user's Diffuse setup: where their data is stored, " +
      "how many tracks, playlists and facets they have, which audio sources " +
      "are configured, and the queue / processing state when those engines are " +
      "loaded. Call this first when you're unsure what the user's setup looks " +
      "like.",
    inputSchema: { type: "object", properties: {} },
    async execute() {
      const out = await foundation.orchestrator.output();
      await customElements.whenDefined(out.localName);

      const [facets, tracks, playlistItems, settings] = await Promise.all([
        Output.data(out.facets),
        Output.data(out.tracks),
        Output.data(out.playlistItems),
        Output.data(out.settings),
      ]);

      const selected = out.selected?.();
      const storage = selected?.label ??
        selected?.getAttribute?.("label") ?? "Local storage";

      // Only report on engines that are already loaded — orientation
      // shouldn't spin up the playback or processing stacks.
      const queueEl = foundation.signals.engine.queue();
      const playback = queueEl
        ? {
          active: queueEl.now()?.id ?? null,
          upcoming: queueEl.future().length,
          history: queueEl.past().length,
        }
        : undefined;

      const sourcesEl = foundation.signals.orchestrator.sources();
      const sources = sourcesEl
        ? Object.entries(sourcesEl.sources()).map(([scheme, list]) => ({
          scheme,
          sources: list.length,
          disabled: list.filter((s) => sourcesEl.isDisabled(s.uri)).length,
        }))
        : undefined;

      const processEl = foundation.signals.orchestrator.processTracks();
      const processing = processEl
        ? { processing: processEl.isProcessing(), ...processEl.progress() }
        : undefined;

      return ok(JSON.stringify({
        group: foundation.GROUP,
        storage,
        tracks: tracks.length,
        playlists: gather(playlistItems).size,
        facets: facets.length,
        settings: settings.length,
        sources,
        playback,
        processing,
      }, null, 2));
    },
  });

  await register({
    name: "list-sources",
    annotations: { readOnlyHint: true },
    description:
      "Lists the audio sources the user's tracks come from, grouped by scheme " +
      "(dropbox, https, local, s3, webdav, …), with their disabled state. " +
      "Disabling a source hides its tracks without deleting them.",
    inputSchema: { type: "object", properties: {} },
    async execute() {
      const sourcesEl = await foundation.orchestrator.sources();
      await customElements.whenDefined(sourcesEl.localName);

      const groups = sourcesEl.sources();
      const trackGroups = groupTracksPerScheme(await collectionTracks());

      const sources = Object.entries(groups).map(([scheme, list]) => ({
        scheme,
        tracks: trackGroups[scheme]?.length ?? 0,
        sources: list.map((source) => ({
          label: source.label,
          uri: source.uri,
          disabled: sourcesEl.isDisabled(source.uri),
        })),
      }));

      return ok(JSON.stringify({
        total: sources.reduce((n, s) => n + s.sources.length, 0),
        sources,
      }, null, 2));
    },
  });

  await register({
    name: "set-source-enabled",
    description:
      "Enables or disables an audio source by its URI (from list-sources). " +
      "Disabled sources' tracks stay saved but are hidden from interfaces and " +
      "excluded from automatic queue filling and processing.",
    inputSchema: {
      type: "object",
      properties: {
        uri: { type: "string", description: "Source URI from list-sources." },
        enabled: { type: "boolean", description: "True to enable, false to disable." },
      },
      required: ["uri", "enabled"],
    },
    async execute({ uri, enabled }) {
      const sourcesEl = await foundation.orchestrator.sources();
      await customElements.whenDefined(sourcesEl.localName);

      const all = Object.values(sourcesEl.sources()).flat();
      const source = all.find((s) => s.uri === uri) ??
        all.find((s) => s.uri.split("?")[0] === uri.split("?")[0]);
      if (!source) {
        return ok(`No source with URI "${uri}" — use list-sources for the available sources.`);
      }

      if (sourcesEl.isDisabled(source.uri) === !enabled) {
        return ok(`The source "${source.label}" is already ${enabled ? "enabled" : "disabled"}.`);
      }

      await sourcesEl.toggle(source.uri);
      return ok(`Source "${source.label}" ${enabled ? "enabled" : "disabled"}.`);
    },
  });

  await register({
    name: "process-library",
    description:
      "Processes the user's audio sources into tracks — reads tags, stats and " +
      "artwork metadata from the files. Runs in the background by default " +
      "(large libraries take a while): call again without 'wait' to check " +
      "progress. Only processes tracks whose sources changed; 'onlyUris' " +
      "limits it to specific source URIs (matched by prefix).",
    inputSchema: {
      type: "object",
      properties: {
        wait: {
          type: "boolean",
          description: "Wait for processing to complete before responding.",
          default: false,
        },
        onlyUris: {
          type: "array",
          items: { type: "string" },
          description: "Only process tracks from these source URIs.",
        },
      },
    },
    async execute({ wait, onlyUris }) {
      const orchestrator = await foundation.orchestrator.processTracks({
        disableWhenReady: true,
      });
      await customElements.whenDefined(orchestrator.localName);

      const status = () => JSON.stringify({
        processing: orchestrator.isProcessing(),
        ...orchestrator.progress(),
      }, null, 2);

      // Already running (e.g. by the prelude in another tab): report or wait.
      if (orchestrator.isProcessing()) {
        if (!wait) return ok(status());

        for (let i = 0; i < 100 && orchestrator.isProcessing(); i++) {
          await new Promise((resolve) => setTimeout(resolve, 500));
        }
        return ok(status());
      }

      if (!wait) {
        // Fire and forget — processing continues in the page; the tool can be
        // called again to check progress.
        orchestrator
          .process(onlyUris?.length ? onlyUris : undefined)
          .catch((e) => console.warn("WebMCP: processing failed", e));

        return ok(JSON.stringify({ started: true }, null, 2));
      }

      await orchestrator.process(onlyUris?.length ? onlyUris : undefined);
      return ok(status());
    },
  });

  await register({
    name: "add-audio",
    description:
      "Adds an audio file to the user's library by downloading it from a URL " +
      "and caching it locally — the same pipeline as the File Manager facet: "+
      "the file shows up there and is processed into a playable track " +
      "automatically. The URL must be fetchable from the page (public or " +
      "CORS-enabled — e.g. a local file server with Access-Control-Allow-Origin).",
    inputSchema: {
      type: "object",
      properties: {
        url: { type: "string", description: "URL of the audio file to download." },
        filename: {
          type: "string",
          description:
            "Filename to store it as (with extension, e.g. \"Song.mp3\"). " +
            "Derived from the URL when omitted.",
        },
      },
      required: ["url"],
    },
    async execute({ url, filename }) {
      if (typeof url !== "string" || !/^https?:\/\//.test(url)) {
        return ok("Provide an 'http(s)://' URL.");
      }

      const name = (filename ?? decodeURIComponent(
        new URL(url).pathname.split("/").pop() ?? "",
      )).trim();
      if (!name) return ok("Provide a 'filename' (the URL doesn't include one).");
      if (!isAudioFile(name)) {
        return ok(
          `\"${name}\" doesn't look like an audio file. Supported extensions: ` +
            ".flac .m4a .mp3 .mp4 .ogg .opus .wav .webm — provide 'filename' to set it.",
        );
      }

      const inputConfigurator = await foundation.configurator.input();
      await customElements.whenDefined(inputConfigurator.localName);

      // Fetch in the page's context — subject to CORS.
      let bytes;
      try {
        const res = await fetch(url);
        if (!res.ok) {
          return ok(`The URL responded ${res.status} ${res.statusText}.`);
        }
        bytes = await res.arrayBuffer();
      } catch {
        return ok(
          `Could not fetch "${url}" from the page. The URL must be publicly ` +
            "reachable or send CORS headers (Access-Control-Allow-Origin).",
        );
      }

      if (bytes.byteLength === 0) return ok("The URL returned an empty file.");
      if (bytes.byteLength > 250 * 1024 * 1024) {
        return ok("The file is larger than the 250MB limit.");
      }

      const file = new File([bytes], name);
      const existing = await collectionTracks();

      // Cache locally (ephemeral cache), like the File Manager's dropzone.
      let uri;
      try {
        uri = await inputConfigurator.cacheBlob(file);
      } catch (err) {
        // The input bundle prelude wires the cache asynchronously; retry once.
        await new Promise((resolve) => setTimeout(resolve, 1500));
        try {
          uri = await inputConfigurator.cacheBlob(file);
        } catch {
          return ok(
            `Caching failed: ${err instanceof Error ? err.message : "unknown error"}.`,
          );
        }
      }

      // Content-addressed: the same file is never added twice.
      if (existing.some((t) => t.uri === uri)) {
        return ok(`\"${name}\" is already in the library.`);
      }

      const now = new Date().toISOString();

      /** @type {Track} */
      const track = {
        $type: "sh.diffuse.output.track",
        id: TID.now(),
        createdAt: now,
        updatedAt: now,
        ephemeral: true,
        uri,
      };

      const out = await foundation.orchestrator.output();
      await out.tracks.save([...existing, track]);

      // Process the new track right away (tags, stats) so it becomes playable.
      const orchestrator = await foundation.orchestrator.processTracks({
        disableWhenReady: true,
      });
      if (!orchestrator.isProcessing()) {
        orchestrator
          .process([uri])
          .catch((e) => console.warn("WebMCP: processing failed", e));
      }

      return ok(JSON.stringify({
        added: name,
        id: track.id,
        uri,
        sizeBytes: bytes.byteLength,
        processing: "started — the track is playable once it's done; check with process-library",
      }, null, 2));
    },
  });

  if (failed) {
    console.error(
      `WebMCP: ${failed} registration(s) failed, ${registered} succeeded — ` +
        `some Diffuse tools may be missing.`,
    );
  } else if (registered) {
    console.info(
      `WebMCP: registered the Diffuse tools on ${contexts.length} host(s).`,
    );
  }
}
