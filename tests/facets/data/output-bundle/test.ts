import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("facets/data/output-bundle", () => {
  it("atproto appends an atproto output element to output", async () => {
    const result = await testWeb(async () => {
      const { atproto } = await import(
        "~/facets/data/output-bundle/index.inline.js"
      );

      const outputChildren: string[] = [];
      const configuratorChildren: string[] = [];
      const mockOutput = {
        append: (el: Element) => outputChildren.push(el.localName),
        outputConfigurator: {
          append: (el: Element) => configuratorChildren.push(el.localName),
        },
      };

      atproto(mockOutput as never);

      return { outputChildren, configuratorChildren };
    });

    expect(result.outputChildren).toContain("dor-atproto");
    expect(result.outputChildren).toContain("dtor-atproto-sync");
  });

  it("atproto appends a passkey refiner to outputConfigurator", async () => {
    const result = await testWeb(async () => {
      const { atproto } = await import(
        "~/facets/data/output-bundle/index.inline.js"
      );

      const configuratorChildren: string[] = [];
      const mockOutput = {
        append: (_el: Element) => {},
        outputConfigurator: {
          append: (el: Element) => configuratorChildren.push(el.localName),
        },
      };

      atproto(mockOutput as never);

      return configuratorChildren;
    });

    expect(result).toContain("dtor-track-uri-passkey");
  });

  it("atproto sets correct ids and attributes on its elements", async () => {
    const result = await testWeb(async () => {
      const { atproto } = await import(
        "~/facets/data/output-bundle/index.inline.js"
      );

      const appended: { id: string; localName: string }[] = [];
      const configuratorAppended: { id: string; localName: string }[] = [];

      const mockOutput = {
        append: (el: Element) =>
          appended.push({ id: el.id, localName: el.localName }),
        outputConfigurator: {
          append: (el: Element) =>
            configuratorAppended.push({ id: el.id, localName: el.localName }),
        },
      };

      atproto(mockOutput as never);

      return { appended, configuratorAppended };
    });

    const atprotoOut = result.appended.find((e) => e.localName === "dor-atproto");
    const atprotoSync = result.appended.find((e) => e.localName === "dtor-atproto-sync");
    const passkey = result.configuratorAppended.find((e) => e.localName === "dtor-track-uri-passkey");

    expect(atprotoOut?.id).toBe("do-output__dor-atproto");
    expect(atprotoSync?.id).toBe("do-output__dtor-atproto-sync");
    expect(passkey?.id).toBe("do-output__dc-output__atproto");
  });

  it("s3 appends an s3 output element to output", async () => {
    const result = await testWeb(async () => {
      const { s3 } = await import(
        "~/facets/data/output-bundle/index.inline.js"
      );

      const outputChildren: string[] = [];
      const mockOutput = {
        append: (el: Element) => outputChildren.push(el.localName),
        outputConfigurator: {
          append: (_el: Element) => {},
        },
      };

      s3(mockOutput as never);

      return outputChildren;
    });

    expect(result).toContain("dob-s3");
  });

  it("s3 appends a dasl-sync transformer to outputConfigurator", async () => {
    const result = await testWeb(async () => {
      const { s3 } = await import(
        "~/facets/data/output-bundle/index.inline.js"
      );

      const configuratorChildren: string[] = [];
      const mockOutput = {
        append: (_el: Element) => {},
        outputConfigurator: {
          append: (el: Element) => configuratorChildren.push(el.localName),
        },
      };

      s3(mockOutput as never);

      return configuratorChildren;
    });

    expect(result).toContain("dtob-dasl-sync");
  });

  it("s3 sets correct ids on its elements", async () => {
    const result = await testWeb(async () => {
      const { s3 } = await import(
        "~/facets/data/output-bundle/index.inline.js"
      );

      const appended: { id: string; localName: string }[] = [];
      const configuratorAppended: { id: string; localName: string }[] = [];

      const mockOutput = {
        append: (el: Element) =>
          appended.push({ id: el.id, localName: el.localName }),
        outputConfigurator: {
          append: (el: Element) =>
            configuratorAppended.push({ id: el.id, localName: el.localName }),
        },
      };

      s3(mockOutput as never);

      return { appended, configuratorAppended };
    });

    const s3Out = result.appended.find((e) => e.localName === "dob-s3");
    const s3Sync = result.configuratorAppended.find((e) => e.localName === "dtob-dasl-sync");

    expect(s3Out?.id).toBe("do-output__dob-s3");
    expect(s3Sync?.id).toBe("do-output__dc-output__s3");
  });
});
