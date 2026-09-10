import { dotenvRun } from "@dotenv-run/esbuild";
import lume from "lume/mod.ts";

import brotli from "lume/plugins/brotli.ts";
import { compress as compressBrotli } from "lume/deps/brotli.ts";
import esbuild from "lume/plugins/esbuild.ts";
import postcss from "lume/plugins/postcss.ts";
import sourceMaps from "lume/plugins/source_maps.ts";

import * as path from "@std/path";
import { ensureDirSync } from "@std/fs/ensure-dir";
import { existsSync } from "@std/fs/exists";
import { walkSync } from "@std/fs/walk";
import { buildTileCar } from "./tasks/tile-car.ts";
import { nodeModulesPolyfillPlugin } from "esbuild-plugins-node-modules-polyfill";
import { wasmLoader } from "esbuild-plugin-wasm";
import autoprefixer from "autoprefixer";
import cssnano from "cssnano";

import { Uint8ArrayReader, Uint8ArrayWriter, ZipWriter } from "@zip-js/zip-js";

import { create as createCID } from "~/common/cid.js";
import { generateManifest } from "./tasks/cem.js";

const site = lume({
  dest: "./dist",
  src: "./src",
  server: {
    debugBar: false,
    middlewares: [],
  },
});

export default site;

////////////////////////////////////////////
// JS
////////////////////////////////////////////

site.use(esbuild({
  extensions: [".js"],
  options: {
    alias: {
      "@automerge/automerge": "https://esm.sh/@automerge/automerge@^3.2.3",
      "@panproto/core": "https://esm.sh/@panproto/core@0.72.0",
    },
    bundle: true,
    format: "esm",
    minify: true,
    external: ["@awesome.me/webawesome/*"],
    platform: "browser",
    plugins: [
      // @ts-ignore
      dotenvRun({
        files: [".env"],
      }),
      // Force @atcute/uint8array to use the browser entry (dist/index.js)
      // instead of the Node entry (dist/index.node.js) which imports from
      // node:crypto. The @deno/loader Workspace defaults to platform "node",
      // causing the "node" export condition to match before "default".
      {
        name: "atcute-uint8array-browser",
        setup(build) {
          build.onLoad(
            { filter: /@atcute\+uint8array.*index\.node\.js$/ },
            async (args) => {
              const browserPath = args.path.replace(
                "index.node.js",
                "index.js",
              );
              const contents = await Deno.readTextFile(browserPath);
              return { contents, loader: "js" };
            },
          );
        },
      },
      {
        name: "atcute-tid-browser",
        setup(build) {
          build.onLoad(
            { filter: /@atcute\+tid.*random-node\.js$/ },
            async (args) => {
              const browserPath = args.path.replace(
                "random-node.js",
                "random-web.js",
              );
              const contents = await Deno.readTextFile(browserPath);
              return { contents, loader: "js" };
            },
          );
        },
      },
      {
        name: "atcute-multibase-browser",
        setup(build) {
          build.onLoad(
            { filter: /@atcute[+/]multibase.*-node\.js$/ },
            async (args) => {
              const browserPath = args.path.replace(
                "-node.js",
                "-web.js",
              );
              const contents = await Deno.readTextFile(browserPath);
              return { contents, loader: "js" };
            },
          );
        },
      },
      // nanoid ships a browser entry (index.browser.js) but esbuild resolves
      // the default condition (index.js) which uses Buffer.allocUnsafe.
      {
        name: "nanoid-browser",
        setup(build) {
          build.onLoad(
            { filter: /nanoid\/index\.js$/ },
            async (args) => {
              const browserPath = args.path.replace(
                "index.js",
                "index.browser.js",
              );
              const contents = await Deno.readTextFile(browserPath);
              return { contents, loader: "js" };
            },
          );
        },
      },
      nodeModulesPolyfillPlugin({
        fallback: "empty",
        modules: [],
      }),
      wasmLoader(),
    ],
    splitting: true,
    target: "esnext",
  },
}));

site.add([".js"]);

// Every bundled facet (`src/facets/**/index.html` + `facet.js`) is packaged into
// a `.tile` CAR by `buildTileCars`, which embeds `/` (index.html), `/facet.js`,
// and `/facet.css` (when present). Those embedded files are served exclusively
// from the tile at runtime, so keep them out of the regular build output — the
// loose copies in dist would otherwise be redundant. `*.facet.js` files are
// ignored wholesale so esbuild doesn't try to bundle them (for tile facets the
// script lives inside the `.tile` CAR instead). `SKILL.md` is packaged into the
// diffuse-facet skill rather than the site build.
const tileFacetEmbeddedPaths = new Set<string>();
for (const entry of walkSync("./src/facets", { includeDirs: true })) {
  if (!entry.isDirectory) continue;
  const indexHtml = path.join(entry.path, "index.html");
  const facetJs = path.join(entry.path, "facet.js");
  if (!existsSync(indexHtml) || !existsSync(facetJs)) continue;

  const base = `/${path.relative("./src", entry.path)}`;
  tileFacetEmbeddedPaths.add(base + "/index.html");
  if (existsSync(path.join(entry.path, "facet.css"))) {
    tileFacetEmbeddedPaths.add(base + "/facet.css");
  }
}

site.ignore(
  (p) =>
    p.endsWith("facet.js") ||
    p.endsWith("SKILL.md") ||
    tileFacetEmbeddedPaths.has(p),
);

////////////////////////////////////////////
// CSS
////////////////////////////////////////////

site.use(postcss({
  plugins: [
    autoprefixer(),
    cssnano({
      preset: "default",
    }),
  ],
}));

site.add([".css"]);

site.remoteFile(
  "vendor/98.css",
  import.meta.resolve("./node_modules/98.css/dist/98.css"),
);

// panproto (`@panproto/core`) loads its WASM lazily at browser runtime during a
// schema write-back. Serve the module's binary so the browser can fetch it
// without loading it into the Deno/build context.
site.remoteFile(
  "panproto_wasm_bg.wasm",
  import.meta.resolve(
    "./node_modules/@panproto/core/dist/panproto_wasm_bg.wasm",
  ),
);
site.add([".wasm"]);

////////////////////////////////////////////
// BINARY ASSETS
////////////////////////////////////////////

site.add("/favicons", "/");
site.add("/fonts");
site.add("/images");
site.add("/testing");
site.add([".woff2"]);

site.remoteFile(
  "vendor/ms_sans_serif.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif.woff2",
  ),
);

site.remoteFile(
  "vendor/ms_sans_serif_bold.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif_bold.woff2",
  ),
);

site.remoteFile(
  "fonts/98.css/ms_sans_serif.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif.woff2",
  ),
);

site.remoteFile(
  "fonts/98.css/ms_sans_serif_bold.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif_bold.woff2",
  ),
);

////////////////////////////////////////////
// DEFINITIONS
////////////////////////////////////////////

site.add("/definitions");

// HELPERS

site.filter("facetURI", (text) => {
  if (text.includes("://")) {
    return text;
  } else {
    return `diffuse://${text}`;
  }
});

site.filter("facetLoaderURL", (text) => {
  let key = "path";

  if (text.includes("://")) {
    key = "uri";
  }

  return `l/?${key}=${encodeURIComponent(text)}`;
});

////////////////////////////////////////////
// PHOSPHOR ICONS
////////////////////////////////////////////

function phosphor(path: string) {
  site.remoteFile(
    `vendor/@phosphor-icons/web/${path}`,
    import.meta.resolve(`./node_modules/@phosphor-icons/web/src/${path}`),
  );

  site.add(`vendor/@phosphor-icons/web/${path}`);
}

["bold", "duotone", "fill", "light", "regular", "light"].forEach((v) => {
  const f = v === "regular" ? "" : `-${v[0].toUpperCase()}${v.slice(1)}`;
  phosphor(`${v}/selection.json`);
  phosphor(`${v}/style.css`);
  phosphor(`${v}/Phosphor${f}.svg`);
  phosphor(`${v}/Phosphor${f}.ttf`);
  phosphor(`${v}/Phosphor${f}.woff`);
  phosphor(`${v}/Phosphor${f}.woff2`);
});

////////////////////////////////////////////
// WEB AWESOME
////////////////////////////////////////////

for (
  const f of walkSync("./node_modules/@awesome.me/webawesome/dist-cdn/", {
    includeDirs: false,
  })
) {
  const relativePath = f.path.replace(
    /^node_modules\/@awesome\.me\/webawesome\/dist-cdn\//,
    "",
  );

  const destPath = `vendor/@awesome.me/webawesome/${relativePath}`;

  site.remoteFile(
    destPath,
    import.meta.resolve(
      `./node_modules/@awesome.me/webawesome/dist-cdn/${relativePath}`,
    ),
  );

  site.copy(destPath);
}

////////////////////////////////////////////
// MISC
////////////////////////////////////////////

site.add([".html"]);
site.add([".json"]);
site.add([".webmanifest"]);

site.script("copy-type-defs", () => {
  for (
    const f of walkSync(
      "./src/",
      { includeDirs: false, exts: [".d.ts"] },
    )
  ) {
    const dest = "dist/" + f.path.replace(/^src\//, "");
    const dir = path.dirname(dest);
    ensureDirSync(dir);
    Deno.copyFileSync(f.path, dest);
  }
});

// SKILLS

site.remoteFile(
  "skills/diffuse-facet/docs/architecture.txt",
  import.meta.resolve("./docs/ARCHITECTURE.md"),
);

site.remoteFile(
  "skills/diffuse-facet/docs/foundation.js",
  import.meta.resolve("./src/common/foundation.js"),
);

site.remoteFile(
  "skills/diffuse-facet/example/index.html",
  import.meta.resolve(
    "./src/facets/themes/blur/artwork-controller/facet/index.html",
  ),
);

site.add("skills/diffuse-facet/docs/architecture.txt");
site.add("skills/diffuse-facet/docs/foundation.js");
site.add("skills/diffuse-facet/example/index.html");
site.add("/definitions", "/skills/diffuse-facet/docs/definitions");
site.copy("skills/diffuse-facet/SKILL.md");
site.add("skills");

site.addEventListener("afterBuild", () => {
  const destDirs = [
    "dist/definitions",
    "dist/skills/diffuse-facet/docs/definitions",
  ];
  for (const f of walkSync("./lexicons/", { includeDirs: false })) {
    const rel = path.relative("./lexicons", f.path);
    for (const destDir of destDirs) {
      const dest = path.join(destDir, rel);
      ensureDirSync(path.dirname(dest));
      Deno.copyFileSync(f.path, dest);
    }
  }
});

site.addEventListener("afterBuild", async () => {
  const skillsDir = "dist/skills/diffuse-facet";
  const zipWriter = new ZipWriter(new Uint8ArrayWriter());

  for (const entry of walkSync(skillsDir, { includeDirs: false })) {
    if (entry.path.endsWith(".br")) continue;
    await zipWriter.add(
      "diffuse-facet/" + entry.path.slice(skillsDir.length + 1),
      new Uint8ArrayReader(Deno.readFileSync(entry.path)),
    );
  }

  Deno.writeFileSync("dist/skills/diffuse-facet.zip", await zipWriter.close());
});

////////////////////////////////////////////
// CUSTOM ELEMENTS MANIFEST
////////////////////////////////////////////

// Generate `dist/custom-elements.json` (+ brotli sidecar) as a build artifact.
// Runs in `afterBuild`, before `writeFileTree` below, so the manifest is
// included in the service-worker file tree and available offline.
site.addEventListener(
  "afterBuild",
  () => generateManifest(import.meta.dirname!),
);

////////////////////////////////////////////
// FILE TREE
////////////////////////////////////////////

async function buildFileTree(
  dir: string,
  prefix = "",
): Promise<Record<string, string>> {
  const RAW = 0x55;
  const tree: Record<string, string> = {};

  for (const entry of Deno.readDirSync(dir)) {
    const entryPath = path.join(dir, entry.name);
    const entryKey = prefix ? `${prefix}/${entry.name}` : entry.name;
    if (entry.isDirectory) {
      Object.assign(tree, await buildFileTree(entryPath, entryKey));
    } else {
      const data = Deno.readFileSync(entryPath);
      tree[entryKey] = await createCID(RAW, data);
    }
  }

  return tree;
}

async function writeFileTree() {
  const swDist = "./dist/service-worker.js";

  // Remove stale brotli'd SW so Caddy never serves the old compressed version
  // while the plain file has been updated.
  try {
    Deno.removeSync(`${swDist}.br`);
  } catch { /* already gone */ }

  const tree = await buildFileTree("dist/");

  // The SW is not included in its own embedded tree — the browser manages the
  // SW lifecycle independently and never fetches it through the SW's handler.
  delete tree["service-worker.js"];
  delete tree["service-worker.js.br"];

  // Remove any .br sidecar that is older than its plain counterpart — this
  // catches cases where brotli compression did not re-run after an asset
  // changed (e.g. incremental afterUpdate builds), so the server falls back
  // to the fresh plain file instead of serving stale compressed bytes.
  for (const key of Object.keys(tree)) {
    if (key.endsWith(".br")) continue;
    const plainPath = `./dist/${key}`;
    const brPath = `${plainPath}.br`;
    try {
      const plainMtime = Deno.statSync(plainPath).mtime;
      const brMtime = Deno.statSync(brPath).mtime;
      if (plainMtime && brMtime && brMtime < plainMtime) {
        Deno.removeSync(brPath);
        delete tree[`${key}.br`];
      }
    } catch { /* .br sidecar doesn't exist — nothing to do */ }
  }

  const sorted = Object.fromEntries(
    Object.keys(tree).sort().map((k) => [k, tree[k]]),
  );

  // Inject the file tree into the compiled SW and stamp with a build ID so
  // the browser always detects an update on each build.
  //
  // The placeholder "__FILE_TREE__" may survive as-is or may be folded by
  // esbuild into `JSON.parse("__FILE_TREE__")` → `"__FILE_TREE__"`. Both
  // patterns are replaced with the actual object literal so that `FILE_TREE`
  // is always an object in the running SW, never a string.
  const treeJson = JSON.stringify(sorted);
  const buildId = crypto.randomUUID();
  const swSource = Deno.readTextFileSync(swDist)
    .replace(/\n\/\/ @build \S+\n$/, "")
    .replace('JSON.parse("__FILE_TREE__")', () => treeJson)
    .replace('"__FILE_TREE__"', () => treeJson)
    .replace('"__BUILD_ID__"', () => JSON.stringify(buildId));
  const swStamped = `${swSource}\n// @build ${buildId}\n`;
  Deno.writeTextFileSync(swDist, swStamped);
}

site.addEventListener("afterBuild", writeFileTree);
site.addEventListener("afterUpdate", writeFileTree);

////////////////////////////////////////////
// BUILD TILE CARS FOR FACETS
////////////////////////////////////////////

// For every bundled facet (`src/facets/**/facet.js` next to an `index.html`, or
// an `index.vto` that Lume renders to `index.html`), build a DASL-style `.tile`
// CAR (index at `/`, facet.js at `/facet.js`) and write it into the build output
// next to the facet.
//
// The embedded files are served exclusively from the tile, so once the tile is
// written the loose copies in `dist` are removed — they would otherwise be
// redundant. For `index.html` facets the regular build already excludes them
// (see `site.ignore` above); here we also catch `.vto` facets whose rendered
// page is produced by Lume before this `afterBuild` hook runs.
async function buildTileCars() {
  const facetsDir = "src/facets";
  const distDir = "dist/facets";

  for (const entry of walkSync(facetsDir, { includeDirs: true })) {
    if (!entry.isDirectory) continue;
    const facetJs = path.join(entry.path, "facet.js");
    if (!existsSync(facetJs)) continue;

    // The tile index comes from a static `index.html`, or — when the facet is
    // authored as a `.vto` template — from the page Lume already rendered
    // (which bakes in any template data, e.g. the facet picker options).
    const indexHtml = path.join(entry.path, "index.html");
    const indexVto = path.join(entry.path, "index.vto");
    let indexContent;
    if (existsSync(indexHtml)) {
      indexContent = Deno.readTextFileSync(indexHtml);
    } else if (existsSync(indexVto)) {
      const rel = path.relative(facetsDir, entry.path);
      const rendered = path.join(distDir, rel, "index.html");
      if (!existsSync(rendered)) continue;
      indexContent = Deno.readTextFileSync(rendered);
    } else {
      continue;
    }

    /** @type {Record<string, { content: string }>} */
    const files: Record<string, { content: string }> = {
      "/": { content: indexContent },
      "/facet.js": { content: Deno.readTextFileSync(facetJs) },
    };
    const facetCss = path.join(entry.path, "facet.css");
    if (existsSync(facetCss)) {
      files["/facet.css"] = { content: Deno.readTextFileSync(facetCss) };
    }

    const car = await buildTileCar(files, {});

    const rel = path.relative(facetsDir, entry.path);
    const outDir = path.join(distDir, rel);
    ensureDirSync(outDir);
    Deno.writeFileSync(path.join(outDir, "index.tile"), car);

    // The tile CAR is fetched over the network by the loader, so give it a
    // brotli `.br` sidecar like the html/css assets. Caddy's `precompressed`
    // serves it with `Content-Encoding: br` and the browser (and service
    // worker, via `fetch`) transparently decompresses — the loader and the
    // content-address cache never see the compressed bytes, only the tile's
    // original bytes, so CIDs stay stable. Written in the same pass as the
    // tile so the sidecar can never go stale.
    Deno.writeFileSync(
      path.join(outDir, "index.tile.br"),
      compressBrotli(car, undefined, 6),
    );

    // Drop the loose embedded files (and any brotli/source-map sidecars) now
    // that they're packaged inside the tile.
    for (const name of ["index.html", "facet.js", "facet.css"]) {
      for (const suffix of ["", ".br", ".map"]) {
        try {
          Deno.removeSync(path.join(outDir, name + suffix));
        } catch { /* already gone */ }
      }
    }
  }
}

site.addEventListener("afterBuild", buildTileCars);
site.addEventListener("afterUpdate", buildTileCars);

////////////////////////////////////////////
// COMPRESSION
////////////////////////////////////////////

site.use(brotli());
site.use(sourceMaps());

////////////////////////////////////////////
// VERSION
////////////////////////////////////////////

// Expose the current package version (`deno.jsonc`) to the templates, so any
// page can display which version of the app it is. On `diffuse.sh` the version
// is also derived from the URL at runtime (see `version-upgrade.js`), which
// overrides this value when they differ.
const denoConfig = Deno.readTextFileSync("./deno.jsonc");
site.data(
  "appVersion",
  /^\s*"version"\s*:\s*"([^"]+)"\s*,?\s*$/m.exec(denoConfig)?.[1],
);
