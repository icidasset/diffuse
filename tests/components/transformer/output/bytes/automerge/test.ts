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
});
