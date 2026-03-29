import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/configurator/input", () => {
  it("listCached returns an empty array initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.listCached();
    });

    expect(result).toEqual([]);
  });

  it("consult with an unsupported scheme returns supported: false", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.consult("ipfs://QmSomeCid");
    });

    expect(result.supported).toBe(false);
  });

  it("resolve with an unsupported scheme returns undefined", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      const r = await configurator.resolve({ uri: "ipfs://QmSomeCid" });
      return r ?? null;
    });

    expect(result).toBe(null);
  });

  it("detach with no matching input returns all tracks unchanged", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);

      const tracks: Track[] = [
        { $type: "sh.diffuse.output.track", id: "t1", uri: "ipfs://Qm1" },
        { $type: "sh.diffuse.output.track", id: "t2", uri: "ipfs://Qm2" },
      ];

      return configurator.detach({ fileUriOrScheme: "ipfs", tracks });
    });

    expect(result.map((t) => t.id)).toEqual(["t1", "t2"]);
  });

  it("groupConsult marks unsupported schemes as unavailable", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);

      return configurator.groupConsult([
        "ipfs://QmA",
        "ipfs://QmB",
      ]);
    });

    expect(result["ipfs"]?.available).toBe(false);
    expect(result["ipfs"]?.uris).toEqual(["ipfs://QmA", "ipfs://QmB"]);
  });

  it("artwork with an unsupported scheme returns null", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.artwork("ipfs://QmSomeCid");
    });

    expect(result).toBe(null);
  });

  it("artwork delegates to the appropriate input and returns null", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const HttpsInput = await import(
        "~/components/input/https/element.js"
      );

      const configurator = new mod.CLASS();
      const httpsInput = new HttpsInput.CLASS();
      configurator.appendChild(httpsInput);
      document.body.append(configurator);

      return configurator.artwork("https://example.com/audio.mp3");
    });

    expect(result).toBe(null);
  });

  it("inputs maps child input elements by their SCHEME", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const HttpsInput = await import(
        "~/components/input/https/element.js"
      );

      const configurator = new mod.CLASS();
      const httpsInput = new HttpsInput.CLASS();
      configurator.appendChild(httpsInput);
      document.body.append(configurator);

      return Object.keys(configurator.inputs());
    });

    expect(result).toContain("https");
  });

  it("consult with an HTTPS URI delegates to the HTTPS input", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const HttpsInput = await import(
        "~/components/input/https/element.js"
      );

      const configurator = new mod.CLASS();
      const httpsInput = new HttpsInput.CLASS();
      configurator.appendChild(httpsInput);
      document.body.append(configurator);

      return configurator.consult("https://example.com/audio.mp3");
    });

    expect(result.supported).toBe(true);
  });

  it("resolve with an HTTPS URI returns a url via the HTTPS input", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/input/element.js"
      );
      const HttpsInput = await import(
        "~/components/input/https/element.js"
      );

      const configurator = new mod.CLASS();
      const httpsInput = new HttpsInput.CLASS();
      configurator.appendChild(httpsInput);
      document.body.append(configurator);

      return configurator.resolve({ uri: "https://example.com/audio.mp3" });
    });

    expect(result).not.toBe(null);
    if (result && "url" in result) {
      expect(result.url).toBe("https://example.com/audio.mp3");
    }
  });
});
