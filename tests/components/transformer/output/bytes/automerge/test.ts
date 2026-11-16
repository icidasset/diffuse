// @ts-nocheck -- the stub output and the evaluated browser function are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/bytes/automerge", () => {
  it("syncs over a plain append-only output", async () => {
    // The stub implements *only* the standard output interface — no `appends`,
    // no `packs`, no `entryCids`. If the transformer needed any of those, this
    // test wouldn't even load.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );
      const doc = await import(
        "~/components/transformer/output/bytes/automerge/doc.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      const join = (chunks) => {
        const total = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
        const out = new Uint8Array(total);
        let offset = 0;
        for (const chunk of chunks) {
          out.set(chunk, offset);
          offset += chunk.length;
        }
        return out;
      };

      /** An append-only `OutputElement<Uint8Array>` with no extra API. */
      class StubOutput extends HTMLElement {
        chunks = Object.fromEntries(names.map((name) => [name, []]));
        saves = [];

        ready() {
          return true;
        }

        append(name, bytes) {
          this.chunks[name].push(bytes);
          return bytes.length;
        }
      }

      for (const name of names) {
        Object.defineProperty(StubOutput.prototype, name, {
          get() {
            const base = this;
            return {
              collection: () => {
                const chunks = base.chunks[name];
                return {
                  state: "loaded",
                  data: chunks.length === 0 ? undefined : join(chunks),
                };
              },
              reload: () => Promise.resolve(),
              save: (bytes) => {
                base.saves.push({ name, bytes });
                base.append(name, bytes);
                return Promise.resolve();
              },
            };
          },
        });
      }

      customElements.define("stub-append-output", StubOutput);

      const output = document.createElement("stub-append-output");
      output.id = "stub-base";
      document.body.append(output);

      const track = (id) => ({
        $type: "sh.diffuse.output.track",
        id,
        uri: `https://example.test/${id}`,
      });

      /** @param {() => boolean} predicate */
      const waitFor = async (predicate) => {
        for (let i = 0; i < 300; i++) {
          if (predicate()) return true;
          await new Promise((resolve) => setTimeout(resolve, 10));
        }
        return false;
      };

      const tracks = () => {
        const collection = transformer.tracks.collection();
        return collection.state === "loaded" ? collection.data : undefined;
      };

      // Another device already published a whole document (a snapshot).
      const published = Array.from({ length: 50 }, (_, i) => track(`t${i}`));
      const remote = doc.changeDocument(doc.createDocument(), published).doc;
      const remoteHeads = doc.headsOf(remote);
      const snapshot = doc.encodeDocument(remote);
      output.append("tracks", snapshot);

      const transformer = new mod.CLASS();
      transformer.id = "stub-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-base");
      document.body.append(transformer);

      const firstPullOk = await waitFor(() => tracks()?.length === 50);
      if (!firstPullOk) return { firstPullOk, count: tracks()?.length };

      // Saving appends one entry — a delta, because the pull told the
      // transformer which heads the remote already has.
      await transformer.tracks.save([...published, track("local")]);

      // Pushes run off the document queue now, so let the write land.
      for (let i = 0; i < 100 && output.chunks.tracks.length < 2; i++) {
        await new Promise((resolve) => setTimeout(resolve, 10));
      }

      const pushed = output.chunks.tracks.at(-1);

      let pushedIsDelta = false;
      try {
        doc.adoptDocument(pushed);
      } catch {
        pushedIsDelta = true;
      }

      // A new delta from another device arrives; a pull applies it off the log.
      const later = doc.changeDocument(remote, [...published, track("other")])
        .doc;
      output.append("tracks", doc.changesSince(later, remoteHeads));

      await transformer.tracks.reload();
      const secondPullOk = await waitFor(() => tracks()?.length === 52);

      const ids = (tracks() ?? []).map((item) => item.id);

      return {
        firstPullOk,
        secondPullOk,
        saves: output.saves.filter((save) => save.name === "tracks").length,
        pushedIsDelta,
        pushedLength: pushed.length,
        snapshotLength: snapshot.length,
        hasLocal: ids.includes("local"),
        hasOther: ids.includes("other"),
      };
    });

    expect(result.firstPullOk).toBe(true);
    expect(result.saves).toBe(1);

    // The write was a change chunk, much smaller than a document.
    expect(result.pushedIsDelta).toBe(true);
    expect(result.pushedLength * 2).toBeLessThan(result.snapshotLength);

    // A delta written by another device is applied from the same log.
    expect(result.secondPullOk).toBe(true);
    expect(result.hasLocal).toBe(true);
    expect(result.hasOther).toBe(true);
  });

  it("stays loading until the output has an answer", async () => {
    // An output that hasn't read anything yet must not be mistaken for an empty
    // collection — otherwise the UI flashes an empty dashboard.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      class PendingOutput extends HTMLElement {
        ready() {
          return true;
        }
      }

      for (const name of names) {
        Object.defineProperty(PendingOutput.prototype, name, {
          get() {
            return {
              collection: () => ({ state: "loading" }),
              reload: () => Promise.resolve(),
              save: () => Promise.resolve(),
            };
          },
        });
      }

      customElements.define("stub-pending-output", PendingOutput);

      const output = document.createElement("stub-pending-output");
      output.id = "stub-pending";
      document.body.append(output);

      const transformer = new mod.CLASS();
      transformer.id = "stub-pending-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-pending");
      document.body.append(transformer);

      // Give the loader and the pull effects a chance to settle.
      await new Promise((resolve) => setTimeout(resolve, 300));

      return transformer.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("stops loading when the output can never read", async () => {
    // Signed out, or no encryption key: the output will never answer, so the
    // UI must not spin forever.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      class UnreadyOutput extends HTMLElement {
        ready() {
          return false;
        }
      }

      for (const name of names) {
        Object.defineProperty(UnreadyOutput.prototype, name, {
          get() {
            return {
              collection: () => ({ state: "loading" }),
              reload: () => Promise.resolve(),
              save: () => Promise.resolve(),
            };
          },
        });
      }

      customElements.define("stub-unready-output", UnreadyOutput);

      const output = document.createElement("stub-unready-output");
      output.id = "stub-unready";
      document.body.append(output);

      const transformer = new mod.CLASS();
      transformer.id = "stub-unready-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-unready");
      document.body.append(transformer);

      const states = () => {
        const collection = transformer.tracks.collection();
        return collection.state;
      };

      const initially = states();

      for (let i = 0; i < 100 && states() === "loading"; i++) {
        await new Promise((resolve) => setTimeout(resolve, 20));
      }

      return { initially, settled: states() };
    });

    // It waits a moment (so a slow-but-working output isn't mistaken for an
    // empty one), then falls back to an empty library.
    expect(result.initially).toBe("loading");
    expect(result.settled).toBe("loaded");
  });

  it("serves a synced collection from the local copy", async () => {
    // A collection that has synced before is shown immediately — even when it
    // is empty — instead of waiting for the first read.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );
      const { signal } = await import("~/common/signal.js");

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      class StubOutput extends HTMLElement {
        chunks = Object.fromEntries(names.map((name) => [name, []]));
        version = signal(0);
        reports = "loaded";

        ready() {
          return true;
        }
      }

      for (const name of names) {
        Object.defineProperty(StubOutput.prototype, name, {
          get() {
            const base = this;
            return {
              collection: () => {
                base.version.get();
                return { state: base.reports };
              },
              reload: () => Promise.resolve(),
              save: () => Promise.resolve(),
            };
          },
        });
      }

      customElements.define("stub-local-copy-output", StubOutput);

      const output = document.createElement("stub-local-copy-output");
      output.id = "stub-local-copy";
      document.body.append(output);

      const namespace = `test-${crypto.randomUUID()}`;

      const first = new mod.CLASS();
      first.id = "stub-local-copy-first";
      first.setAttribute("namespace", namespace);
      first.setAttribute("output-selector", "#stub-local-copy");
      document.body.append(first);

      // A save writes the local copy and records the push cursor.
      await new Promise((resolve) => setTimeout(resolve, 200));
      await first.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "kept",
          uri: "https://example.test/kept",
        },
      ]);

      // Second instance, same namespace, and an output that never answers.
      output.reports = "loading";

      const second = new mod.CLASS();
      second.id = "stub-local-copy-second";
      second.setAttribute("namespace", namespace);
      second.setAttribute("output-selector", "#stub-local-copy");
      document.body.append(second);

      // Well below the 750 ms fallback, so this must come from the local copy.
      await new Promise((resolve) => setTimeout(resolve, 400));

      const collection = second.tracks.collection();
      const firstCollection = first.tracks.collection();


      return {
        state: collection.state,
        ids: collection.state === "loaded"
          ? collection.data.map((track) => track.id)
          : null,
        ready: second.ready(),
        firstState: firstCollection.state,
        firstIds: firstCollection.state === "loaded"
          ? firstCollection.data.map((track) => track.id)
          : null,
        reports: output.reports,

      };
    });

    expect(result.state).toBe("loaded");
    expect(result.ids).toEqual(["kept"]);
  });

  it("stops loading when the local cache never answers", async () => {
    // The local output reads through a worker; a stuck worker used to leave
    // every collection loading, with nothing logged.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      class StubOutput extends HTMLElement {
        ready() {
          return true;
        }
      }

      for (const name of names) {
        Object.defineProperty(StubOutput.prototype, name, {
          get() {
            return {
              collection: () => ({ state: "loading" }),
              reload: () => Promise.resolve(),
              save: () => Promise.resolve(),
            };
          },
        });
      }

      customElements.define("stub-stuck-local-output", StubOutput);

      const output = document.createElement("stub-stuck-local-output");
      output.id = "stub-stuck-local";
      document.body.append(output);

      const transformer = new mod.CLASS();
      transformer.id = "stub-stuck-local-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-stuck-local");
      document.body.append(transformer);

      // Wedge the local cache before `#initialise` runs.
      const local = transformer.shadowRoot?.querySelector("dop-indexed-db") ??
        transformer.querySelector("dop-indexed-db");

      for (const name of names) {
        local[name].reload = () => new Promise(() => {});
      }

      const started = performance.now();
      const state = () => transformer.tracks.collection().state;

      while (state() === "loading" && performance.now() - started < 5000) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }

      return {
        ready: transformer.ready(),
        elapsed: Math.round(performance.now() - started),
        state: state(),
      };
    });

    expect(result.ready).toBe(true);
    // Bounded by the local-read timeout, not by a hanging worker.
    expect(result.elapsed).toBeLessThan(5000);
    expect(result.state).toBe("loaded");
  });

  it("does not wait forever for a slow first read", async () => {
    // A read that never lands must not hold the first paint hostage.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      class HangingOutput extends HTMLElement {
        ready() {
          return true;
        }
      }

      for (const name of names) {
        Object.defineProperty(HangingOutput.prototype, name, {
          get() {
            return {
              collection: () => ({ state: "loading" }),
              reload: () => new Promise(() => {}),
              save: () => Promise.resolve(),
            };
          },
        });
      }

      customElements.define("stub-hanging-output", HangingOutput);

      const output = document.createElement("stub-hanging-output");
      output.id = "stub-hanging";
      document.body.append(output);

      const transformer = new mod.CLASS();
      transformer.id = "stub-hanging-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-hanging");
      document.body.append(transformer);

      const state = () => transformer.tracks.collection().state;

      const initially = state();

      for (let i = 0; i < 200 && state() === "loading"; i++) {
        await new Promise((resolve) => setTimeout(resolve, 20));
      }

      return { initially, settled: state() };
    });

    expect(result.initially).toBe("loading");
    expect(result.settled).toBe("loaded");
  });

  it("applies a log the output hands over without a reload", async () => {
    // This is the live path: the output re-reads after a channel hint and
    // updates its own signal; the transformer has to notice without anyone
    // calling `reload()` on it.
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );
      const doc = await import(
        "~/components/transformer/output/bytes/automerge/doc.js"
      );
      const { signal } = await import("~/common/signal.js");

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      const join = (chunks) => {
        const total = chunks.reduce((sum, chunk) => sum + chunk.length, 0);
        const out = new Uint8Array(total);
        let offset = 0;
        for (const chunk of chunks) {
          out.set(chunk, offset);
          offset += chunk.length;
        }
        return out;
      };

      class StubLog extends HTMLElement {
        chunks = Object.fromEntries(names.map((name) => [name, []]));
        // The real elements hand out signal readers, so a push has to be
        // observable without a reload.
        version = signal(0);

        ready() {
          return true;
        }

        append(name, bytes) {
          this.chunks[name].push(bytes);
          this.version.value = this.version.value + 1;
        }
      }

      for (const name of names) {
        Object.defineProperty(StubLog.prototype, name, {
          get() {
            const base = this;
            return {
              collection: () => {
                base.version.get();

                return {
                  state: "loaded",
                  data: base.chunks[name].length === 0
                    ? undefined
                    : join(base.chunks[name]),
                };
              },
              reload: () => Promise.resolve(),
              save: (bytes) => {
                base.append(name, bytes);
                return Promise.resolve();
              },
            };
          },
        });
      }

      customElements.define("stub-live-log", StubLog);

      const log = document.createElement("stub-live-log");
      log.id = "stub-live-log";
      document.body.append(log);

      const track = (id) => ({
        $type: "sh.diffuse.output.track",
        id,
        uri: `https://example.test/${id}`,
      });

      const waitFor = async (predicate) => {
        for (let i = 0; i < 300; i++) {
          if (predicate()) return true;
          await new Promise((resolve) => setTimeout(resolve, 10));
        }
        return false;
      };

      const remote = doc.changeDocument(doc.createDocument(), [track("a")]).doc;
      const remoteHeads = doc.headsOf(remote);
      log.append("tracks", doc.encodeDocument(remote));

      const transformer = new mod.CLASS();
      transformer.id = "stub-live-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-live-log");
      document.body.append(transformer);

      const tracks = () => {
        const collection = transformer.tracks.collection();
        return collection.state === "loaded" ? collection.data : undefined;
      };

      const firstOk = await waitFor(() => tracks()?.length === 1);

      // Another device appends a delta. Nothing calls reload(): the log's
      // value changes as if a hint had made it re-read.
      const later = doc.changeDocument(remote, [track("a"), track("b")]).doc;
      log.append("tracks", doc.changesSince(later, remoteHeads));

      const liveOk = await waitFor(() => tracks()?.length === 2);

      return {
        firstOk,
        liveOk,
        ids: (tracks() ?? []).map((item) => item.id).sort(),
      };
    });

    expect(result.firstOk).toBe(true);
    expect(result.liveOk).toBe(true);
    expect(result.ids).toEqual(["a", "b"]);
  });

  it("works over a replace-only output when `snapshot` is set", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/bytes/automerge/element.js"
      );
      const doc = await import(
        "~/components/transformer/output/bytes/automerge/doc.js"
      );

      const names = [
        "collaborations",
        "facets",
        "playlistItems",
        "settings",
        "tracks",
      ];

      /** An output that keeps only the newest value, like S3 or Dropbox. */
      class StubValueOutput extends HTMLElement {
        values = Object.fromEntries(names.map((name) => [name, undefined]));
        saves = [];

        ready() {
          return true;
        }
      }

      for (const name of names) {
        Object.defineProperty(StubValueOutput.prototype, name, {
          get() {
            const base = this;
            return {
              collection: () => ({
                state: "loaded",
                data: base.values[name],
              }),
              reload: () => Promise.resolve(),
              save: (bytes) => {
                base.saves.push({ name, bytes });
                base.values[name] = bytes;
                return Promise.resolve();
              },
            };
          },
        });
      }

      customElements.define("stub-value-output", StubValueOutput);

      const output = document.createElement("stub-value-output");
      output.id = "stub-value";
      document.body.append(output);

      const track = (id) => ({
        $type: "sh.diffuse.output.track",
        id,
        uri: `https://example.test/${id}`,
      });

      /** @param {() => boolean} predicate */
      const waitFor = async (predicate) => {
        for (let i = 0; i < 300; i++) {
          if (predicate()) return true;
          await new Promise((resolve) => setTimeout(resolve, 10));
        }
        return false;
      };

      const tracks = (transformer) => {
        const collection = transformer.tracks.collection();
        return collection.state === "loaded" ? collection.data : undefined;
      };

      const published = Array.from({ length: 20 }, (_, i) => track(`t${i}`));
      const remote = doc.changeDocument(doc.createDocument(), published).doc;
      const publishedBytes = doc.encodeDocument(remote);
      output.values.tracks = publishedBytes;

      const transformer = new mod.CLASS();
      transformer.id = "stub-value-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-value");
      transformer.setAttribute("snapshot", "");
      document.body.append(transformer);

      const pullOk = await waitFor(() => tracks(transformer)?.length === 20);

      await transformer.tracks.save([...published, track("local")]);

      for (
        let i = 0;
        i < 100 && output.values.tracks === publishedBytes;
        i++
      ) {
        await new Promise((resolve) => setTimeout(resolve, 10));
      }

      const pushed = output.values.tracks;

      // The whole value must be a document: a fresh reader has no history.
      let standalone = true;
      try {
        doc.adoptDocument(pushed);
      } catch {
        standalone = false;
      }

      // A second device with no cache reads the same collection.
      const other = new mod.CLASS();
      other.id = "stub-value-transformer-2";
      other.setAttribute("namespace", `test-${crypto.randomUUID()}-b`);
      other.setAttribute("output-selector", "#stub-value");
      document.body.append(other);

      const secondDeviceOk = await waitFor(() =>
        tracks(other)?.length === 21
      );

      return { pullOk, standalone, secondDeviceOk };
    });

    expect(result.pullOk).toBe(true);
    expect(result.standalone).toBe(true);
    expect(result.secondDeviceOk).toBe(true);
  });
});
