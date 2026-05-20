import { dotenvRun } from "@dotenv-run/esbuild";
import lume from "lume/mod.ts";

import brotli from "lume/plugins/brotli.ts";
import esbuild from "lume/plugins/esbuild.ts";
import postcss from "lume/plugins/postcss.ts";
import sourceMaps from "lume/plugins/source_maps.ts";

import * as path from "@std/path";
import { ensureDirSync } from "@std/fs/ensure-dir";
import { walkSync } from "@std/fs/walk";
import { nodeModulesPolyfillPlugin } from "esbuild-plugins-node-modules-polyfill";
import { wasmLoader } from "esbuild-plugin-wasm";
import autoprefixer from "autoprefixer";
import cssnano from "cssnano";

import { Uint8ArrayReader, Uint8ArrayWriter, ZipWriter } from "@zip-js/zip-js";

import { create as createCID } from "~/common/cid.js";

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

// *.inline.js files are inlined into their companion HTML at build/serve time.
// Exclude them from the regular build so esbuild doesn't try to bundle them.
site.ignore((p) => p.endsWith(".inline.js") || p.endsWith("SKILL.md"));

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
  import.meta.resolve("./src/facets/data/sources/index.html"),
);

site.add("skills/diffuse-facet/docs/architecture.txt");
site.add("skills/diffuse-facet/docs/foundation.js");
site.add("skills/diffuse-facet/example/index.html");
site.add("/definitions", "/skills/diffuse-facet/docs/definitions");
site.copy("skills/diffuse-facet/SKILL.md");
site.add("skills");

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
  const RAW = 0x55;

  // Stamp the built SW with a unique build ID so the browser always detects a
  // SW update on each build, not just when the SW source itself changes.
  // Do this before buildFileTree (and before brotli's stale-sidecar cleanup)
  // so the stamped version is the one captured in the tree CID.  Also delete
  // any brotli'd copy of the *un*stamped SW so Caddy's precompressed-br
  // serving never delivers it to the browser's update checker.
  const swDist = "./dist/service-worker.js";
  try { Deno.removeSync(`${swDist}.br`); } catch { /* already gone */ }
  const swSource = Deno.readTextFileSync(swDist)
    .replace(/\n\/\/ @build \S+\n$/, "");
  const swStamped = `${swSource}\n// @build ${crypto.randomUUID()}\n`;
  Deno.writeTextFileSync(swDist, swStamped);

  const tree = await buildFileTree("dist/");

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

  tree["service-worker.js"] = await createCID(
    RAW,
    new TextEncoder().encode(swStamped),
  );

  // Exclude file-tree.json from its own contents (self-referential).
  delete tree["file-tree.json"];
  delete tree["file-tree.json.br"];

  const sorted = Object.fromEntries(
    Object.keys(tree).sort().map((k) => [k, tree[k]]),
  );

  Deno.writeTextFileSync(
    "./dist/file-tree.json",
    JSON.stringify(sorted, null, 2),
  );
}

site.addEventListener("afterBuild", writeFileTree);
site.addEventListener("afterUpdate", writeFileTree);

////////////////////////////////////////////
// INLINE JS FOR FACETS
////////////////////////////////////////////

const SCRIPT_SRC_RE =
  /<script type="module" src="([^"]+\.inline\.js)"><\/script>/;

site.process([".html"], (pages) => {
  for (const page of pages) {
    const content = page.text;
    if (!content) continue;
    const match = SCRIPT_SRC_RE.exec(content);
    if (!match) continue;

    const jsPath = path.join("src", match[1]);
    try {
      page.text = htmlWithInlineJs({ content, jsPath, match: match[0] });
    } catch {
      // leave as-is if the source file can't be read
    }
  }
});

function htmlWithInlineJs({ content, match, jsPath }: {
  content: string;
  match: string;
  jsPath: string;
}): string {
  const js =
    Deno.readTextFileSync(jsPath).split("\n").map((line) => `  ${line}`).join(
      "\n",
    ).trimEnd() + "\n";
  return content.replace(match, `<script type="module">\n${js}</script>`);
}

////////////////////////////////////////////
// COMPRESSION
////////////////////////////////////////////

site.use(brotli());
site.use(sourceMaps());
