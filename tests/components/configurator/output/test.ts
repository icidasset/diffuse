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

  it("falls back to the default output when the selected output disconnects", async () => {
    const result = await testWeb(async () => {
      const idb = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const { CLASS } = await import(
        "~/components/configurator/output/element.js"
      );
      const { signal } = await import("~/common/signal.js");

      const suffix = crypto.randomUUID();
      const ready = signal(true);

      const def = new idb.CLASS();
      def.id = "disconnect-default";
      def.setAttribute("namespace", `disconnect-default-${suffix}`);

      const remote = new idb.CLASS();
      remote.id = "disconnect-remote";
      remote.setAttribute("namespace", `disconnect-remote-${suffix}`);
      remote.ready = () => ready.get();

      const configurator = new CLASS();
      configurator.setAttribute("default", def.id);
      configurator.append(def, remote);
      document.body.append(configurator);

      // Let the configurator finish its initial (async) setup before changing
      // the selection, or its connectedCallback would overwrite it.
      await new Promise((resolve) => setTimeout(resolve, 100));

      const waitFor = async (predicate: () => boolean) => {
        const deadline = Date.now() + 3_000;
        while (Date.now() < deadline) {
          if (predicate()) return true;
          await new Promise((resolve) => setTimeout(resolve, 20));
        }

        return predicate();
      };

      await configurator.select(remote.id);

      const selectedRemote = await waitFor(() =>
        configurator.selected()?.id === remote.id
      );

      ready.set(false);

      const fellBack = await waitFor(() =>
        configurator.selected() === null && configurator.hasSelected() === false
      );

      return {
        selectedRemote,
        fellBack,
        selected: configurator.selected()?.id ?? null,
        hasSelected: configurator.hasSelected(),
      };
    });

    expect(result).toEqual({
      selectedRemote: true,
      fellBack: true,
      selected: null,
      hasSelected: false,
    });
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
