import foundation from "~/common/foundation.js";
import * as TID from "@atcute/tid";
import * as Output from "~/common/output.js";
import { resolveFacetTile } from "~/common/loader.js";
import {
  filterByPlaylist,
  gather,
  orderByPlaylist,
} from "~/common/playlist.js";
import {
  htmlFacetTile,
  tileFromFiles,
  tileResourceEntries,
} from "~/common/tiles.js";

/**
 * @import {Facet, PlaylistItem, Track} from "~/definitions/types.d.ts"
 */

const FACET_TYPE = "sh.diffuse.output.facet";

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

////////////////////////////////////////////
// PLAYBACK
////////////////////////////////////////////

/**
 * The queue and audio engines, created on first use (reused when the
 * interface facet already spun them up).
 *
 * @returns {Promise<{
 *   queue: import("~/components/engine/queue/element.js").CLASS,
 *   audio: import("~/components/engine/audio/element.js").CLASS,
 * }>}
 */
async function engines() {
  const [queue, audio] = await Promise.all([
    foundation.engine.queue(),
    foundation.engine.audio(),
  ]);

  await Promise.all([
    customElements.whenDefined(queue.localName),
    customElements.whenDefined(audio.localName),
  ]);

  return { queue, audio };
}

/**
 * Plays tracks immediately: the first added track becomes the active one, the
 * rest line up behind it. The engine's `play()` silently no-ops on an item
 * that isn't supplied yet, so when audio isn't already playing (in which case
 * the queue-audio orchestrator continues playback on its own) we wait for the
 * orchestrator to resolve & supply the track before pressing play.
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

  const audioId = trackIds[0];
  for (let i = 0; i < 50; i++) {
    if (audio.state(audioId)) break;
    await new Promise((resolve) => setTimeout(resolve, 100));
  }

  audio.play({ audioId });
  return !!audio.state(audioId);
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
    description:
      "Reads a single facet: its metadata plus its full source code. Single-file " +
      "facets return 'html'; facets that ship extra files (modules, stylesheets) " +
      "also return 'files', a list of { path, content }. Use this to read a facet " +
      "before improving its code. Facet code can be large — prefer list-facets " +
      "first to find the right one.",
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
    description:
      "Searches the user's track collection by title, artist, album artist or " +
      "album (case-insensitive substring match). Returns matching tracks with " +
      "the ids needed by queue-tracks / play-tracks. Omit 'query' to sample the " +
      "collection instead.",
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
      const q = (query ?? "").trim().toLowerCase();

      const matches = q
        ? tracks.filter((t) => trackHaystack(t).includes(q))
        : sortTracks(tracks);

      const max = Math.min(Math.max(limit ?? 25, 1), 100);
      return ok(JSON.stringify({
        total: matches.length,
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
