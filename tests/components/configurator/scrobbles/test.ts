import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/scrobbles", () => {
  it("scrobblers returns empty array when there are no children", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);
      return configurator.scrobblers().length;
    });

    expect(result).toBe(0);
  });

  it("nowPlaying resolves without throwing when there are no active scrobblers", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "t1",
        uri: "https://example.com/audio.mp3",
      };

      await configurator.nowPlaying(track);
      return true;
    });

    expect(result).toBe(true);
  });

  it("scrobble resolves without throwing when there are no active scrobblers", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();
      document.body.append(configurator);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "t1",
        uri: "https://example.com/audio.mp3",
      };

      await configurator.scrobble(track, Date.now());
      return true;
    });

    expect(result).toBe(true);
  });

  it("scrobblers includes child elements that have the scrobble interface", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();

      // Mock scrobbler with the required interface
      const mock = document.createElement("div");
      (mock as any).isAuthenticated = () => true;
      (mock as any).nowPlaying = async () => {};
      (mock as any).scrobble = async () => {};
      configurator.appendChild(mock);

      document.body.append(configurator);

      return configurator.scrobblers().length;
    });

    expect(result).toBe(1);
  });

  it("scrobblers excludes children that do not have the scrobble interface", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();

      // Plain element without scrobble methods
      const plain = document.createElement("div");
      configurator.appendChild(plain);

      document.body.append(configurator);

      return configurator.scrobblers().length;
    });

    expect(result).toBe(0);
  });

  it("nowPlaying calls all authenticated scrobblers", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();

      const calls: string[] = [];

      const auth = document.createElement("div");
      (auth as any).isAuthenticated = () => true;
      (auth as any).nowPlaying = async () => { calls.push("auth"); };
      (auth as any).scrobble = async () => {};

      const unauth = document.createElement("div");
      (unauth as any).isAuthenticated = () => false;
      (unauth as any).nowPlaying = async () => { calls.push("unauth"); };
      (unauth as any).scrobble = async () => {};

      configurator.appendChild(auth);
      configurator.appendChild(unauth);
      document.body.append(configurator);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "t1",
        uri: "https://example.com/audio.mp3",
      };

      await configurator.nowPlaying(track);
      return calls;
    });

    expect(result).toEqual(["auth"]);
  });

  it("scrobble calls all authenticated scrobblers", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/configurator/scrobbles/element.js"
      );
      const configurator = new mod.CLASS();

      const calls: string[] = [];

      const auth = document.createElement("div");
      (auth as any).isAuthenticated = () => true;
      (auth as any).nowPlaying = async () => {};
      (auth as any).scrobble = async () => { calls.push("auth"); };

      const unauth = document.createElement("div");
      (unauth as any).isAuthenticated = () => false;
      (unauth as any).nowPlaying = async () => {};
      (unauth as any).scrobble = async () => { calls.push("unauth"); };

      configurator.appendChild(auth);
      configurator.appendChild(unauth);
      document.body.append(configurator);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "t1",
        uri: "https://example.com/audio.mp3",
      };

      await configurator.scrobble(track, Date.now());
      return calls;
    });

    expect(result).toEqual(["auth"]);
  });
});
