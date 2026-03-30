import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/facets/category", () => {
  it("color returns accent-twist-4 for prelude kind", async () => {
    const result = await testWeb(async () => {
      const { color } = await import("~/common/facets/category.js");
      return color({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "prelude" });
    });
    expect(result).toBe("var(--accent-twist-4)");
  });

  it("color returns accent-twist-2 for interactive kind", async () => {
    const result = await testWeb(async () => {
      const { color } = await import("~/common/facets/category.js");
      return color({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "interactive" });
    });
    expect(result).toBe("var(--accent-twist-2)");
  });

  it("color returns accent-twist-2 for undefined kind", async () => {
    const result = await testWeb(async () => {
      const { color } = await import("~/common/facets/category.js");
      return color({ $type: "sh.diffuse.output.facet", id: "1", name: "x" });
    });
    expect(result).toBe("var(--accent-twist-2)");
  });

  it("name returns 'feature' for prelude kind", async () => {
    const result = await testWeb(async () => {
      const { name } = await import("~/common/facets/category.js");
      return name({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "prelude" });
    });
    expect(result).toBe("feature");
  });

  it("name returns 'interface' for interactive kind", async () => {
    const result = await testWeb(async () => {
      const { name } = await import("~/common/facets/category.js");
      return name({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "interactive" });
    });
    expect(result).toBe("interface");
  });

  it("name returns 'interface' for undefined kind", async () => {
    const result = await testWeb(async () => {
      const { name } = await import("~/common/facets/category.js");
      return name({ $type: "sh.diffuse.output.facet", id: "1", name: "x" });
    });
    expect(result).toBe("interface");
  });
});

describe("common/facets/constants", () => {
  it("TYPE is sh.diffuse.output.facet", async () => {
    const result = await testWeb(async () => {
      const { TYPE } = await import("~/common/facets/constants.js");
      return TYPE;
    });
    expect(result).toBe("sh.diffuse.output.facet");
  });

  it("STARTING_SET_URIS includes connect facet", async () => {
    const result = await testWeb(async () => {
      const { STARTING_SET_URIS } = await import("~/common/facets/constants.js");
      return STARTING_SET_URIS;
    });
    expect(result).toContain("facets/connect/index.html");
  });

  it("STARTING_SET_URIS includes blur artwork controller", async () => {
    const result = await testWeb(async () => {
      const { STARTING_SET_URIS } = await import("~/common/facets/constants.js");
      return STARTING_SET_URIS;
    });
    expect(result).toContain("themes/blur/artwork-controller/facet/index.html");
  });

  it("STARTING_SET_URIS has 9 entries", async () => {
    const result = await testWeb(async () => {
      const { STARTING_SET_URIS } = await import("~/common/facets/constants.js");
      return STARTING_SET_URIS.length;
    });
    expect(result).toBe(9);
  });
});

describe("common/facets/utils", () => {
  it("facetFromURI sets $type", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      return facet.$type;
    });
    expect(result).toBe("sh.diffuse.output.facet");
  });

  it("facetFromURI sets name and uri", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "My Facet", uri: "facets/test/index.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      return { name: facet.name, uri: facet.uri };
    });
    expect(result.name).toBe("My Facet");
    expect(result.uri).toBe("facets/test/index.html");
  });

  it("facetFromURI with fetchHTML false leaves html and cid undefined", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      return { html: facet.html ?? null, cid: facet.cid ?? null };
    });
    expect(result.html).toBe(null);
    expect(result.cid).toBe(null);
  });

  it("facetFromURI sets prelude kind", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: "prelude", description: undefined },
        { fetchHTML: false },
      );
      return facet.kind;
    });
    expect(result).toBe("prelude");
  });

  it("facetFromURI sets interactive kind", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: "interactive", description: undefined },
        { fetchHTML: false },
      );
      return facet.kind;
    });
    expect(result).toBe("interactive");
  });

  it("facetFromURI sets kind to undefined for unrecognised kind", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: "unknown", description: undefined },
        { fetchHTML: false },
      );
      return facet.kind ?? null;
    });
    expect(result).toBe(null);
  });

  it("facetFromURI sets description", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: undefined, description: "A test facet" },
        { fetchHTML: false },
      );
      return facet.description;
    });
    expect(result).toBe("A test facet");
  });

  it("facetFromURI sets createdAt and updatedAt as matching ISO strings", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      return { createdAt: facet.createdAt, updatedAt: facet.updatedAt };
    });
    expect(result.createdAt).toBeTruthy();
    expect(result.createdAt).toBe(result.updatedAt);
    expect(new Date(result.createdAt!).toISOString()).toBe(result.createdAt);
  });

  it("facetFromURI generates a non-empty string id", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const facet = await facetFromURI(
        { name: "Test", uri: "test.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      return facet.id;
    });
    expect(typeof result).toBe("string");
    expect(result.length).toBeGreaterThan(0);
  });

  it("facetFromURI generates unique ids", async () => {
    const result = await testWeb(async () => {
      const { facetFromURI } = await import("~/common/facets/utils.js");
      const a = await facetFromURI(
        { name: "A", uri: "a.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      const b = await facetFromURI(
        { name: "B", uri: "b.html", kind: undefined, description: undefined },
        { fetchHTML: false },
      );
      return a.id === b.id;
    });
    expect(result).toBe(false);
  });
});

describe("common/facets/prelude", () => {
  it("insertPreludes only inserts prelude kind facets", async () => {
    const result = await testWeb(async () => {
      const { insertPreludes } = await import("~/common/facets/prelude.js");
      const container = document.createElement("div");
      document.body.append(container);

      await insertPreludes(
        [
          { $type: "sh.diffuse.output.facet", id: "1", name: "A", kind: "interactive", html: "<span id='fp-interactive'></span>" },
          { $type: "sh.diffuse.output.facet", id: "2", name: "B", kind: "prelude", html: "<span id='fp-prelude'></span>" },
        ],
        container,
      );

      return {
        hasInteractive: !!container.querySelector("#fp-interactive"),
        hasPrelude: !!container.querySelector("#fp-prelude"),
      };
    });

    expect(result.hasInteractive).toBe(false);
    expect(result.hasPrelude).toBe(true);
  });

  it("insertPreludes sorts preludes alphabetically by name", async () => {
    const result = await testWeb(async () => {
      const { insertPreludes } = await import("~/common/facets/prelude.js");
      const container = document.createElement("div");
      document.body.append(container);

      await insertPreludes(
        [
          { $type: "sh.diffuse.output.facet", id: "1", name: "Zebra", kind: "prelude", html: "<span class='fp-order'>Zebra</span>" },
          { $type: "sh.diffuse.output.facet", id: "2", name: "Alpha", kind: "prelude", html: "<span class='fp-order'>Alpha</span>" },
          { $type: "sh.diffuse.output.facet", id: "3", name: "Mango", kind: "prelude", html: "<span class='fp-order'>Mango</span>" },
        ],
        container,
      );

      return Array.from(container.querySelectorAll(".fp-order")).map((el) => el.textContent);
    });

    expect(result).toEqual(["Alpha", "Mango", "Zebra"]);
  });

  it("insertPreludes defaults to document.body", async () => {
    const result = await testWeb(async () => {
      const { insertPreludes } = await import("~/common/facets/prelude.js");

      await insertPreludes([
        { $type: "sh.diffuse.output.facet", id: "1", name: "Test", kind: "prelude", html: "<span id='fp-body-test'></span>" },
      ]);

      return !!document.body.querySelector("#fp-body-test");
    });

    expect(result).toBe(true);
  });

  it("insertPreludes skips prelude with no html and no uri", async () => {
    const result = await testWeb(async () => {
      const { insertPreludes } = await import("~/common/facets/prelude.js");
      const container = document.createElement("div");
      document.body.append(container);

      await insertPreludes(
        [{ $type: "sh.diffuse.output.facet", id: "1", name: "Empty", kind: "prelude" }],
        container,
      );

      return container.childNodes.length;
    });

    expect(result).toBe(0);
  });
});
