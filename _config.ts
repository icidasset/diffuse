import type { RequestHandler } from "lume/core/server.ts";

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

import { create as createCID } from "~/common/cid.js";

const site = lume({
  dest: "./dist",
  src: "./src",
  server: {
    debugBar: false,
    middlewares: [], // [facetHtmlMiddleware],
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
    external: ["./file-tree.json", "@awesome.me/webawesome/*"],
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
site.ignore((p) => p.endsWith(".inline.js"));

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

site.use(brotli());
site.use(sourceMaps());

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

site.addEventListener("afterBuild", () => {
  // site.run("copy-type-defs");
});

////////////////////////////////////////////
// MIDDLEWARE
////////////////////////////////////////////

// Facet HTML files are HTML fragments fetched via JS, not full pages.
// Serving them as text/plain prevents Lume's dev server from injecting
// its live-reload <script> tag into the fetched content.
//
// Also inlines any <script type="module" src="./foo.inline.js"> references so
// that forked facets contain readable JS rather than an external file reference.
async function facetHtmlMiddleware(
  request: Request,
  next: RequestHandler,
): Promise<Response> {
  const { pathname } = new URL(request.url);
  const isFacetHtml = pathname.endsWith(".html") &&
    !pathname.startsWith("/testing/");
  const response = await next(request);

  if (!isFacetHtml || !response.headers.get("content-type")?.includes("html")) {
    return response;
  }

  let content = await response.text();
  content = await inlineScriptSrc(content);

  const headers = new Headers(response.headers);
  headers.set("content-type", "text/plain; charset=utf-8");
  return new Response(content, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

const SCRIPT_SRC_RE =
  /<script type="module" src="([^"]+\.inline\.js)"><\/script>/;

async function inlineScriptSrc(content: string): Promise<string> {
  const match = SCRIPT_SRC_RE.exec(content);
  if (!match) return content;

  const jsPath = path.join("src", match[1]);
  try {
    return htmlWithInlineJs({ content, jsPath, match: match[0] });
  } catch {
    return content;
  }
}

site.addEventListener("afterBuild", async () => {
  for (
    const f of walkSync("./dist/", { includeDirs: false, exts: [".html"] })
  ) {
    const content = Deno.readTextFileSync(f.path);
    const match = SCRIPT_SRC_RE.exec(content);
    if (!match) continue;

    const jsPath = path.join("src", match[1]);

    try {
      const newContent = htmlWithInlineJs({ content, jsPath, match: match[0] });
      Deno.writeTextFileSync(f.path, newContent);
    } catch {
      // leave as-is if the source file can't be read
    }
  }
});

site.addEventListener("afterBuild", async () => {
  const RAW = 0x55;

  async function buildFileTree(
    dir: string,
    prefix = "",
  ): Promise<Record<string, string>> {
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

  const tree = await buildFileTree("dist/");
  const sorted = Object.fromEntries(
    Object.keys(tree).sort().map((k) => [k, tree[k]]),
  );

  Deno.writeTextFileSync(
    "./dist/file-tree.json",
    JSON.stringify(sorted, null, 2),
  );
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
