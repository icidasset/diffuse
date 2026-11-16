// @ts-nocheck -- the stub output and the evaluated browser function are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/bytes/automerge", () => {
  it("pulls only unseen log entries and pushes a delta", async () => {
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

      // A stand-in for `dob-firkin`: an append-only log that records what the
      // transformer asks for and what it writes.
      let seq = 0;

      class StubLog extends HTMLElement {
        appends = true;
        entries = Object.fromEntries(names.map((name) => [name, []]));
        requested = [];
        saves = [];

        ready() {
          return true;
        }

        entryCids(name) {
          return Promise.resolve(this.entries[name].map((entry) => entry.cid));
        }

        packs(name, cids) {
          this.requested.push(...(cids ?? []));

          const wanted = cids ? new Set(cids) : undefined;
          return Promise.resolve(
            this.entries[name].filter((entry) =>
              !wanted || wanted.has(entry.cid)
            ),
          );
        }

        packCount(name) {
          return Promise.resolve(this.entries[name].length);
        }

        compact(name, snapshot) {
          this.entries[name] = [{ cid: `snapshot-${++seq}`, bytes: snapshot }];
          return Promise.resolve();
        }

        /** Simulate another device appending to the log. */
        append(name, bytes) {
          const cid = `entry-${++seq}`;
          this.entries[name].push({ cid, bytes });
          return cid;
        }
      }

      for (const name of names) {
        Object.defineProperty(StubLog.prototype, name, {
          get() {
            const base = this;
            return {
              collection: () => ({ state: "loaded", data: undefined }),
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

      customElements.define("stub-firkin-log", StubLog);

      const base = document.createElement("stub-firkin-log");
      base.id = "stub-base";
      document.body.append(base);

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

      // Another device already published a full document (a bootstrap or a
      // compacted snapshot), big enough that a delta is obviously smaller.
      const published = Array.from({ length: 50 }, (_, i) => track(`t${i}`));
      const remote = doc.changeDocument(doc.createDocument(), published).doc;
      const remoteHeads = doc.headsOf(remote);
      const remoteCid = base.append("tracks", doc.encodeDocument(remote));

      const transformer = new mod.CLASS();
      transformer.id = "stub-transformer";
      transformer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      transformer.setAttribute("output-selector", "#stub-base");
      document.body.append(transformer);

      const firstPullOk = await waitFor(() => tracks()?.length === 50);
      const firstPull = [...base.requested];

      if (!firstPullOk) {
        return { firstPullOk, firstPull, ids: (tracks() ?? []).length };
      }

      // Saving appends one entry to the log.
      const fullLength = doc.encodeDocument(remote).length;
      await transformer.tracks.save([...published, track("local")]);

      const pushed = base.entries.tracks.at(-1);
      const saves = base.saves.length;

      // A delta can't be loaded on its own (its dependencies are missing); a
      // whole document can.
      let pushedIsDelta = false;
      try {
        doc.adoptDocument(pushed.bytes);
      } catch {
        pushedIsDelta = true;
      }

      // Re-pulling must not re-fetch what was already applied.
      const before = base.requested.length;
      await transformer.tracks.reload();
      const refetched = base.requested.slice(before);

      // A new entry from another device is picked up on the next pull. It is a
      // delta against the published document, so it only applies if this device
      // is on the same lineage.
      const later = doc.changeDocument(remote, [...published, track("other")])
        .doc;
      base.append("tracks", doc.changesSince(later, remoteHeads));

      await transformer.tracks.reload();
      const secondPullOk = await waitFor(() => tracks()?.length === 52);

      const ids = (tracks() ?? []).map((item) => item.id);

      return {
        firstPullOk,
        secondPullOk,
        firstPull,
        remoteCid,
        refetched,
        saves,
        pushedIsDelta,
        pushedLength: pushed.bytes.length,
        fullLength,
        hasLocal: ids.includes("local"),
        hasOther: ids.includes("other"),
      };
    });

    // Only the unseen entry was fetched on the first pull, and the local save
    // appended exactly one entry.
    expect(result.firstPullOk).toBe(true);
    expect(result.firstPull).toEqual([result.remoteCid]);
    expect(result.saves).toBe(1);

    // The push was a change chunk, not a whole document, and it is far smaller.
    expect(result.pushedIsDelta).toBe(true);
    expect(result.pushedLength * 2).toBeLessThan(result.fullLength);

    // Already-applied entries aren't fetched again.
    expect(result.refetched).not.toContain(result.remoteCid);

    // Local and remote changes both survive.
    expect(result.secondPullOk).toBe(true);
    expect(result.hasLocal).toBe(true);
    expect(result.hasOther).toBe(true);
  });
});
