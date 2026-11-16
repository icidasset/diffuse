// @ts-nocheck -- stub outputs are untyped
import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/output (switching)", () => {
  it("carries the local copy in before showing the selected output", async () => {
    const result = await testWeb(async () => {
      const { signal, computed } = await import("~/common/signal.js");
      const mod = await import(
        "~/components/configurator/output/element.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      localStorage.removeItem("diffuse/configurator/output/selected/id");

      /** @param {string} id */
      const makeOutput = (id) => {
        const cols = Object.fromEntries(
          names.map((n) => [n, signal({ state: "loading" })]),
        );

        const tag = `probe-${id}`;
        if (!customElements.get(tag)) {
          customElements.define(
            tag,
            class extends HTMLElement {
              constructor() {
                super();
                this.identifier = tag;

                for (const n of names) {
                  this[n] = {
                    collection: computed(() => cols[n].get()),
                    reload: async () => {},
                    save: async (data) =>
                      cols[n].set({ state: "loaded", data }),
                  };
                }

                this.ready = () => true;
              }
            },
          );
        }

        const el = document.createElement(tag);
        el.id = id;

        return { el, cols };
      };

      const local = makeOutput("local");
      const remote = makeOutput("remote");

      const localFacets = [{ id: "a", name: "Local prelude", kind: "prelude" }];
      const localTracks = [{ id: "t1" }];
      local.cols.facets.set({ state: "loaded", data: localFacets });
      local.cols.tracks.set({ state: "loaded", data: localTracks });

      const configurator = new mod.CLASS();
      configurator.setAttribute("default", "local");
      configurator.append(local.el, remote.el);
      document.body.append(configurator);

      // Let `connectedCallback` find the default output.
      await new Promise((r) => setTimeout(r, 200));

      // The selected output answers (empty), so the carry can proceed.
      setTimeout(() => {
        for (const n of names) {
          remote.cols[n].set({ state: "loaded", data: [] });
        }
      }, 100);

      const pending = configurator.select("remote");

      // While the switch is being prepared, the previous source still answers.
      const during = {
        facets: configurator.facets.collection(),
        tracks: configurator.tracks.collection(),
      };

      await pending;

      return {
        during,
        after: {
          facets: configurator.facets.collection(),
          tracks: configurator.tracks.collection(),
        },
        remote: {
          facets: remote.cols.facets.get(),
          tracks: remote.cols.tracks.get(),
        },
      };
    });

    // No spinner and no empty library while switching.
    expect(result.during.facets.state).toBe("loaded");
    expect(result.during.facets.data.length).toBe(1);
    expect(result.during.tracks.data).toEqual([{ id: "t1" }]);

    // The selected output now holds the carried copy…
    expect(result.remote.facets).toEqual({
      state: "loaded",
      data: [{ id: "a", name: "Local prelude", kind: "prelude" }],
    });
    expect(result.remote.tracks).toEqual({
      state: "loaded",
      data: [{ id: "t1" }],
    });

    // …and is the one being read.
    expect(result.after.facets.data.length).toBe(1);
    expect(result.after.tracks.data).toEqual([{ id: "t1" }]);
  });

  it("keeps what the selected output already has", async () => {
    const result = await testWeb(async () => {
      const { signal, computed } = await import("~/common/signal.js");
      const mod = await import(
        "~/components/configurator/output/element.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      localStorage.removeItem("diffuse/configurator/output/selected/id");

      /** @param {string} id */
      const makeOutput = (id) => {
        const cols = Object.fromEntries(
          names.map((n) => [n, signal({ state: "loading" })]),
        );

        const tag = `probe-${id}`;
        if (!customElements.get(tag)) {
          customElements.define(
            tag,
            class extends HTMLElement {
              constructor() {
                super();
                this.identifier = tag;

                for (const n of names) {
                  this[n] = {
                    collection: computed(() => cols[n].get()),
                    reload: async () => {},
                    save: async (data) =>
                      cols[n].set({ state: "loaded", data }),
                  };
                }

                this.ready = () => true;
              }
            },
          );
        }

        const el = document.createElement(tag);
        el.id = id;

        return { el, cols };
      };

      const local = makeOutput("local");
      const remote = makeOutput("remote");

      local.cols.facets.set({
        state: "loaded",
        data: [{ id: "a", name: "Local prelude", kind: "prelude" }],
      });

      for (const n of names) {
        remote.cols[n].set({ state: "loaded", data: [] });
      }

      remote.cols.facets.set({
        state: "loaded",
        data: [{ id: "b", name: "Remote prelude", kind: "prelude" }],
      });

      const configurator = new mod.CLASS();
      configurator.setAttribute("default", "local");
      configurator.append(local.el, remote.el);
      document.body.append(configurator);

      await new Promise((r) => setTimeout(r, 200));

      await configurator.select("remote");

      return {
        after: configurator.facets.collection(),
        remote: remote.cols.facets.get(),
      };
    });

    // The output's own data is not overwritten.
    expect(result.remote.data).toEqual([
      { id: "b", name: "Remote prelude", kind: "prelude" },
    ]);
    expect(result.after.data).toEqual([
      { id: "b", name: "Remote prelude", kind: "prelude" },
    ]);
  });
});
