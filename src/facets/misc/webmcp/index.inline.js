import foundation from "~/common/foundation.js";
import * as CID from "~/common/cid.js";
import * as TID from "@atcute/tid";
import * as Output from "~/common/output.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

const FACET_TYPE = "sh.diffuse.output.facet";

/** @param {string} text */
const ok = (text) => ({ content: [{ type: "text", text }] });

/** @param {string} html */
async function htmlCID(html) {
  return await CID.create(0x55, new TextEncoder().encode(html));
}

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
 * Registers facet CRUD tools with the browser's agent via WebMCP.
 * No-op on browsers without WebMCP support.
 */
async function registerFacetTools() {
  const modelContext = /** @type {any} */ (document).modelContext;
  if (!modelContext?.registerTool) return;

  /**
   * @param {{
   *   name: string,
   *   description: string,
   *   inputSchema: Record<string, any>,
   *   execute: (args: Record<string, any>) => Promise<unknown>,
   * }} tool
   */
  const register = async (tool) => {
    try {
      await modelContext.registerTool(tool);
    } catch (e) {
      // `NotAllowedError` when the `tools` permission is disabled — degrade silently.
      console.warn("WebMCP tool registration failed:", e);
    }
  };

  // — LIST ————————————————————————————————————————————
  await register({
    name: "list-facets",
    description:
      "Lists the user's Diffuse facets (interfaces and features). Returns id, name, " +
      "kind, description, enabled, favourite, uri and tags — but not the HTML source " +
      "(use get-facet for that). Hidden 'base' facets are excluded by default.",
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

      const summary = facets.map((f) => ({
        id: f.id,
        name: f.name,
        kind: f.kind ?? "interactive",
        description: f.description ?? "",
        enabled: f.enabled ?? true,
        favourite: f.favourite ?? false,
        uri: f.uri,
        tags: f.tags,
        hasHtml: typeof f.html === "string",
      }));

      return ok(JSON.stringify(summary, null, 2));
    },
  });

  // — GET —————————————————————————————————————————————
  await register({
    name: "get-facet",
    description:
      "Returns a single facet by id, including its full HTML source. Use this to read " +
      "a facet before improving its code.",
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
      if (!facet.html && facet.uri) {
        return ok(
          `Facet "${facet.name}" tracks a remote URI (${facet.uri}) and has no local HTML yet.`,
        );
      }
      return ok(JSON.stringify(facet, null, 2));
    },
  });

  // — CREATE ———————————————————————————————————————————
  await register({
    name: "create-facet",
    description:
      "Creates a new Diffuse facet and saves it to the user's collection. Pass the " +
      "complete facet HTML (no <!doctype>/<html>/<head>; the loader wraps it) along with " +
      "a name, kind ('interactive' interface or 'prelude' feature) and description.",
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
        tags: { type: "array", items: { type: "string" }, description: "Optional tags." },
      },
      required: ["name", "html"],
    },
    async execute({ name, kind, description, html, tags }) {
      const now = new Date().toISOString();
      /** @type {Facet} */
      const facet = {
        $type: FACET_TYPE,
        id: TID.now(),
        name,
        kind: kind === "prelude" ? "prelude" : "interactive",
        description: description ?? undefined,
        html,
        cid: await htmlCID(html),
        tags: tags?.length ? tags : undefined,
        createdAt: now,
        updatedAt: now,
      };

      await saveFacet(facet);
      return ok(JSON.stringify({ created: true, id: facet.id, name: facet.name }));
    },
  });

  // — UPDATE ———————————————————————————————————————————
  await register({
    name: "update-facet",
    description:
      "Updates an existing facet by id. Omitted fields are left unchanged; provide " +
      "'html' to replace the facet's source code (typically after improving it).",
    inputSchema: {
      type: "object",
      properties: {
        id: { type: "string", description: "Facet id from list-facets." },
        name: { type: "string" },
        kind: { type: "string", enum: ["interactive", "prelude"] },
        description: { type: "string" },
        html: { type: "string", description: "New complete facet HTML source." },
        enabled: { type: "boolean" },
        tags: { type: "array", items: { type: "string" } },
      },
      required: ["id"],
    },
    async execute({ id, name, kind, description, html, enabled, tags }) {
      const facets = await listFacets();
      const existing = facets.find((f) => f.id === id);
      if (!existing) return ok(`No facet with id "${id}" was found.`);

      /** @type {Facet} */
      const facet = { ...existing };
      if (name !== undefined) facet.name = name;
      if (kind !== undefined) facet.kind = kind === "prelude" ? "prelude" : "interactive";
      if (description !== undefined) facet.description = description;
      if (enabled !== undefined) facet.enabled = enabled;
      if (tags !== undefined) facet.tags = tags.length ? tags : undefined;
      if (html !== undefined) {
        facet.html = html;
        facet.cid = await htmlCID(html);
      }

      await saveFacet(facet); // saveFacet bumps updatedAt
      return ok(JSON.stringify({ updated: true, id: facet.id, name: facet.name }));
    },
  });
}

foundation.setup({ title: "WebMCP | Diffuse" });

const status = document.querySelector("#webmcp-status");

if ("modelContext" in document) {
  await registerFacetTools();
  if (status) {
    status.classList.add("is-ok");
    status.textContent =
      "Registered: list-facets, get-facet, create-facet, update-facet.";
  }
} else if (status) {
  status.classList.add("is-error");
  status.textContent = "WebMCP is not available in this browser.";
}

foundation.ready();
