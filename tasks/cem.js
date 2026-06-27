/**
 * Custom Elements Manifest generation, shared by the standalone `gen:manifest`
 * task and the Lume `afterBuild` hook in `_config.ts`.
 *
 * We analyze the *source* under `src/components/` (which keeps JSDoc, clean
 * class/member names and the `defineElement` registration pattern) but record
 * each module's path as its *compiled* `dist` location, so consumers can link a
 * manifest entry to the actual served JS file. The compiled files themselves
 * are minified, code-split re-export stubs (the real code lives in shared
 * chunks with mangled names), so they can't be analyzed directly.
 *
 * We use the analyzer's programmatic `create()` API rather than its CLI. The
 * CLI auto-loads a `custom-elements-manifest.config.js` via `@web/config-loader`,
 * which currently hangs under Deno, so we drive the analyzer directly here.
 *
 * Diffuse registers its elements with a `defineElement(name, constructor)` helper
 * (see `src/common/element.js`) instead of calling `customElements.define` directly.
 * The `defineElementPlugin` below teaches the analyzer about that pattern:
 *
 * ```js
 * export const NAME = "de-queue";        // the custom element tag name
 * export const CLASS = QueueEngine;       // optional alias of the class
 * defineElement(NAME, QueueEngine);       // or defineElement(NAME, CLASS)
 * ```
 */
// The package's `index.d.ts` is incomplete and doesn't re-export `create`/`ts`,
// though they exist at runtime. See https://github.com/open-wc/custom-elements-manifest
// @ts-ignore: package ships no type declarations for `create` and `ts`
import { create, ts } from "@custom-elements-manifest/analyzer";
import { expandGlob } from "@std/fs";
import { ensureDir } from "@std/fs/ensure-dir";
import { relative, resolve } from "@std/path";
// Reuse Lume's brotli so the manifest is compressed exactly like other assets.
// `CompressionStream` in Deno doesn't support "br", and Lume's brotli plugin
// only compresses *registered* pages (this file is written straight to `dist/`).
import { compress } from "lume/deps/brotli.ts";

const SRC_GLOB = "src/components/**/element.js";

/**
 * Generates `dist/custom-elements.json` (plus a brotli `.br` sidecar) for the
 * elements in `<root>/src/components/` and writes them into `<root>/dist/`.
 *
 * @param {string} root Project root containing `src/` and `dist/`.
 */
export async function generateManifest(root) {
  const distDir = resolve(root, "dist");
  const outfile = resolve(distDir, "custom-elements.json");

  const modules = [];

  for await (const entry of expandGlob(SRC_GLOB, { root })) {
    const srcPath = relative(root, entry.path);
    // Reference the deployed (`dist`) module path rather than the source path:
    // `src/components/.../element.js` -> `components/.../element.js`. The manifest
    // is written into `dist/`, so these paths resolve relative to the site root.
    const distPath = srcPath.replace(/^src\//, "");
    const text = await Deno.readTextFile(entry.path);
    // `setParentNodes: true` is required so node.getSourceFile() resolves during analysis.
    const sourceFile = ts.createSourceFile(
      distPath,
      text,
      ts.ScriptTarget.Latest,
      true,
    );
    modules.push(sourceFile);
  }

  if (modules.length === 0) {
    console.error(`No modules matched "${SRC_GLOB}"`);
    Deno.exit(1);
  }

  const manifest = create({
    modules,
    plugins: [defineElementPlugin()],
    context: { dev: false },
  });

  const json = JSON.stringify(manifest, null, 2) + "\n";

  await ensureDir(distDir);
  await Deno.writeTextFile(outfile, json);
  await Deno.writeFile(
    `${outfile}.br`,
    compress(new TextEncoder().encode(json)),
  );

  console.log(
    `Generated ${
      relative(root, outfile)
    } (+ .br) from ${modules.length} module(s).`,
  );
}

/**
 * Detects `defineElement(tagName, constructor)` calls and registers a
 * `custom-element-definition` export for them, mirroring what the analyzer
 * does for `customElements.define()` out of the box.
 *
 * `tagName` is usually an identifier referencing a `const NAME = "de-queue"`
 * export, and `constructor` is either the class itself or a `const CLASS =
 * RealClass` alias. Both are resolved to their concrete values so the core
 * `linkClassToTagname` plugin can wire the tag name onto the class declaration.
 */
function defineElementPlugin() {
  return {
    name: "DIFFUSE - DEFINE-ELEMENT-CALLS",
    /** @param {{ ts: any; node: any; moduleDoc: any }} ctx */
    analyzePhase({ ts, node, moduleDoc }) {
      if (!ts || node?.kind !== ts.SyntaxKind.CallExpression) return;

      /** @type {any} */
      const expr = node.expression;
      if (expr?.kind !== ts.SyntaxKind.Identifier) return;
      if (expr.text !== "defineElement") return;

      const args = node.arguments;
      if (!args || args.length < 2) return;

      const tagArg = args[0];
      const classArg = args[1];
      if (!tagArg || !classArg) return;

      const sourceFile = node.getSourceFile();
      const tagName = resolveStringLiteral(ts, tagArg, sourceFile);
      const className = resolveClassName(ts, classArg, sourceFile);
      if (!tagName || !className) return;

      const definitionDoc = {
        kind: "custom-element-definition",
        name: tagName,
        declaration: {
          name: className,
          module: moduleDoc.path,
        },
      };

      moduleDoc.exports = [...(moduleDoc.exports || []), definitionDoc];
    },
  };
}

/**
 * Resolves an argument to a string value. Handles string literals directly,
 * and identifiers by looking up a `const <name> = "<value>"` declaration in
 * the same module (e.g. `const NAME = "de-queue"`).
 *
 * @param {any} ts
 * @param {any} node
 * @param {any} sourceFile
 * @returns {string | undefined}
 */
function resolveStringLiteral(ts, node, sourceFile) {
  if (!node) return undefined;
  if (node.kind === ts.SyntaxKind.StringLiteral) return node.text;

  if (node.kind === ts.SyntaxKind.Identifier) {
    return findConstStringValue(ts, node.text, sourceFile);
  }

  return undefined;
}

/**
 * Resolves the class argument to a concrete class name. Handles identifiers
 * that are either a class declaration name (e.g. `QueueEngine`) or a
 * `const CLASS = QueueEngine` alias.
 *
 * @param {any} ts
 * @param {any} node
 * @param {any} sourceFile
 * @returns {string | undefined}
 */
function resolveClassName(ts, node, sourceFile) {
  if (!node) return undefined;

  if (ts.isClassExpression(node) || ts.isClassDeclaration(node)) {
    return node.name?.getText() ?? undefined;
  }

  if (node.kind !== ts.SyntaxKind.Identifier) return undefined;
  const name = node.text;

  if (hasClassDeclaration(ts, name, sourceFile)) return name;

  const aliased = findConstIdentifierValue(ts, name, sourceFile);
  if (aliased && hasClassDeclaration(ts, aliased, sourceFile)) {
    return aliased;
  }

  return name;
}

/**
 * @param {any} ts
 * @param {string} constName
 * @param {any} sourceFile
 * @returns {string | undefined}
 */
function findConstStringValue(ts, constName, sourceFile) {
  let value;
  forEachChild(ts, sourceFile, (node) => {
    if (!ts.isVariableStatement(node)) return;
    for (const decl of node.declarationList?.declarations ?? []) {
      if (decl.name?.getText() !== constName) continue;
      const init = decl.initializer;
      if (init?.kind === ts.SyntaxKind.StringLiteral) value = init.text;
    }
  });
  return value;
}

/**
 * @param {any} ts
 * @param {string} constName
 * @param {any} sourceFile
 * @returns {string | undefined}
 */
function findConstIdentifierValue(ts, constName, sourceFile) {
  let value;
  forEachChild(ts, sourceFile, (node) => {
    if (!ts.isVariableStatement(node)) return;
    for (const decl of node.declarationList?.declarations ?? []) {
      if (decl.name?.getText() !== constName) continue;
      const init = decl.initializer;
      if (init?.kind === ts.SyntaxKind.Identifier) value = init.text;
    }
  });
  return value;
}

/**
 * @param {any} ts
 * @param {string} className
 * @param {any} sourceFile
 * @returns {boolean}
 */
function hasClassDeclaration(ts, className, sourceFile) {
  let found = false;
  forEachChild(ts, sourceFile, (node) => {
    if (ts.isClassDeclaration(node) && node.name?.getText() === className) {
      found = true;
    }
  });
  return found;
}

/**
 * Safe recursive AST walker using TypeScript's `forEachChild`.
 *
 * @param {any} ts
 * @param {any} node
 * @param {(node: any) => void} fn
 */
function forEachChild(ts, node, fn) {
  if (!node) return;
  fn(node);
  ts.forEachChild(
    node,
    (/** @type {any} */ child) => forEachChild(ts, child, fn),
  );
}
