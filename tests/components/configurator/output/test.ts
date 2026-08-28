import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/output", () => {
  it("selected is null initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.selected();
    });

    expect(result).toBe(null);
  });

  it("activated is empty initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return Array.from(configurator.activated());
    });

    expect(result).toEqual([]);
  });

  it("hasSelected returns false when nothing is stored in localStorage", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.hasSelected();
    });

    expect(result).toBe(false);
  });

  it("hasDefault returns false without a default attribute", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.hasDefault();
    });

    expect(result).toBe(false);
  });

  it("hasDefault returns true when the default attribute is set", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      configurator.setAttribute("default", "my-output");
      document.body.append(configurator);
      return configurator.hasDefault();
    });

    expect(result).toBe(true);
  });

  it("select persists the id to localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      await configurator.select("my-output");
      return localStorage.getItem("diffuse/configurator/output/selected/id");
    });

    expect(stored).toBe("my-output");
  });

  it("hasSelected returns true after select", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      await configurator.select("my-output");
      return configurator.hasSelected();
    });

    expect(result).toBe(true);
  });

  it("activated includes id after select", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      await configurator.select("my-output");
      return Array.from(configurator.activated());
    });

    expect(result).toContain("my-output");
  });

  it("deselect removes the id from localStorage", async () => {
    const stored = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      await configurator.select("my-output");
      await configurator.deselect();
      return localStorage.getItem("diffuse/configurator/output/selected/id");
    });

    expect(stored).toBe(null);
  });

  it("hasSelected returns false after deselect", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      await configurator.select("my-output");
      await configurator.deselect();
      return configurator.hasSelected();
    });

    expect(result).toBe(false);
  });

  it("options lists child elements by id and label", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();

      const child = document.createElement("div");
      child.id = "output-a";
      child.setAttribute("label", "Output A");
      configurator.appendChild(child);

      document.body.append(configurator);

      const opts = await configurator.options();
      return opts.map((o) => ({ id: o.id, label: o.label }));
    });

    expect(result).toEqual([{ id: "output-a", label: "Output A" }]);
  });

  it("options returns an empty array when there are no children", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/output/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.options();
    });

    expect(result).toEqual([]);
  });
});
