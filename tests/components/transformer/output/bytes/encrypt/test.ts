// @ts-nocheck -- the stub output and the evaluated browser function are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/bytes/encrypt", () => {
  it("stores ciphertext under an automerge document", async () => {
    const result = await testWeb(async () => {
      const encrypt = await import(
        "~/components/transformer/output/bytes/encrypt/element.js"
      );
      const automerge = await import(
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

      class StubLog extends HTMLElement {
        chunks = Object.fromEntries(names.map((name) => [name, []]));

        ready() {
          return true;
        }

        append(name, bytes) {
          this.chunks[name].push(bytes);
        }
      }

      for (const name of names) {
        Object.defineProperty(StubLog.prototype, name, {
          get() {
            const base = this;
            return {
              collection: () => ({
                state: "loaded",
                data: base.chunks[name].length === 0
                  ? undefined
                  : join(base.chunks[name]),
              }),
              reload: () => Promise.resolve(),
              save: (bytes) => {
                base.append(name, bytes);
                return Promise.resolve();
              },
            };
          },
        });
      }

      customElements.define("stub-encrypt-log", StubLog);

      const log = document.createElement("stub-encrypt-log");
      log.id = "stub-encrypt-log";
      document.body.append(log);

      const key = await crypto.subtle.generateKey(
        { name: "AES-GCM", length: 256 },
        false,
        ["encrypt", "decrypt"],
      );

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

      // One device publishes a document through the encryption element.
      const writer = document.createElement(encrypt.NAME);
      writer.id = "encrypt-writer";
      writer.setAttribute("namespace", `test-${crypto.randomUUID()}`);
      writer.setAttribute("output-selector", "#stub-encrypt-log");
      writer.setKey(key);
      document.body.append(writer);

      const published = Array.from({ length: 10 }, (_, i) => track(`t${i}`));
      const remote = doc.changeDocument(doc.createDocument(), published).doc;
      await writer.tracks.save(doc.encodeDocument(remote));

      // The backing store only ever saw ciphertext.
      const stored = log.chunks.tracks[0];
      const plaintextMagic = [0x85, 0x6f, 0x4a, 0x83];
      const looksPlain = plaintextMagic.every((byte, index) =>
        stored[index] === byte
      );

      // A second device with the same key reads the collection.
      const readerEncrypt = document.createElement(encrypt.NAME);
      readerEncrypt.id = "encrypt-reader";
      readerEncrypt.setAttribute("namespace", `test-${crypto.randomUUID()}-b`);
      readerEncrypt.setAttribute("output-selector", "#stub-encrypt-log");
      readerEncrypt.setKey(key);
      document.body.append(readerEncrypt);

      const reader = document.createElement(automerge.NAME);
      reader.id = "encrypt-reader-transformer";
      reader.setAttribute("namespace", `test-${crypto.randomUUID()}-c`);
      reader.setAttribute("output-selector", "#encrypt-reader");
      document.body.append(reader);

      const readerTracks = () => {
        const collection = reader.tracks.collection();
        return collection.state === "loaded" ? collection.data : undefined;
      };

      const readOk = await waitFor(() => readerTracks()?.length === 10);

      // A wrong key surfaces as an error rather than garbage.
      const otherKey = await crypto.subtle.generateKey(
        { name: "AES-GCM", length: 256 },
        false,
        ["encrypt", "decrypt"],
      );

      const wrongEncrypt = document.createElement(encrypt.NAME);
      wrongEncrypt.id = "encrypt-wrong";
      wrongEncrypt.setAttribute("namespace", `test-${crypto.randomUUID()}-d`);
      wrongEncrypt.setAttribute("output-selector", "#stub-encrypt-log");
      wrongEncrypt.setKey(otherKey);
      document.body.append(wrongEncrypt);

      const wrongOk = await waitFor(() =>
        wrongEncrypt.tracks.collection().state === "error"
      );

      return {
        looksPlain,
        storedLength: stored.length,
        readOk,
        wrongOk,
        ready: reader.ready(),
      };
    });

    expect(result.looksPlain).toBe(false);
    expect(result.storedLength).toBeGreaterThan(0);
    expect(result.readOk).toBe(true);
    expect(result.wrongOk).toBe(true);
  });
});
