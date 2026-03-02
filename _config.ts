import type { RequestHandler } from "lume/core/server.ts";
import { dotenvRun } from "@dotenv-run/esbuild";
import lume from "lume/mod.ts";

import esbuild from "lume/plugins/esbuild.ts";
import postcss from "lume/plugins/postcss.ts";
import purgecss from "lume/plugins/purgecss.ts";
import sourceMaps from "lume/plugins/source_maps.ts";

import * as path from "@std/path";
import { ensureDirSync } from "@std/fs/ensure-dir";
import { walkSync } from "@std/fs/walk";
import { nodeModulesPolyfillPlugin } from "esbuild-plugins-node-modules-polyfill";
import { wasmLoader } from "esbuild-plugin-wasm";

const site = lume({
  dest: "./dist",
  src: "./src",
  server: {
    debugBar: false,
    middlewares: [facetHtmlMiddleware],
  },
});

export default site;

// JS

site.use(esbuild({
  extensions: [".js"],
  options: {
    alias: {
      "@automerge/automerge": "https://esm.sh/@automerge/automerge@^3.2.3",
    },
    bundle: true,
    format: "esm",
    minify: false,
    // outExtension: { ".js": ".min.js" },
    platform: "browser",
    plugins: [
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
            { filter: /@atcute\+multibase.*-node\.js$/ },
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
      nodeModulesPolyfillPlugin({
        globals: {
          process: true,
          Buffer: true,
        },
      }),
      wasmLoader(),
    ],
    splitting: true,
    target: "esnext",
  },
}));

site.add([".js"]);

// CSS

site.use(postcss());
// site.use(purgecss());
site.add([".css"]);

site.remoteFile(
  "styles/vendor/98.css",
  import.meta.resolve("./node_modules/98.css/dist/98.css"),
);

// BINARY ASSETS

site.add("/favicons", "/");
site.add("/fonts");
site.add("/images");
site.add([".woff2"]);

site.remoteFile(
  "styles/vendor/ms_sans_serif.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif.woff2",
  ),
);

site.remoteFile(
  "styles/vendor/ms_sans_serif_bold.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif_bold.woff2",
  ),
);

site.remoteFile(
  "fonts/ms_sans_serif.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif.woff2",
  ),
);

site.remoteFile(
  "fonts/ms_sans_serif_bold.woff2",
  import.meta.resolve(
    "./node_modules/98.css/fonts/converted/ms_sans_serif_bold.woff2",
  ),
);

// DEFINITIONS

site.add("/definitions");

// HELPERS

site.filter("facetOrThemeURI", (text) => {
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

  return `facets/l/?${key}=${encodeURIComponent(text)}`;
});

site.filter("themeLoaderURL", (text) => {
  let key = "path";

  if (text.includes("://")) {
    key = "uri";
  }

  return `themes/l/?${key}=${encodeURIComponent(text)}`;
});

// PHOSPHOR ICONS

function phosphor(path: string) {
  site.remoteFile(
    `styles/vendor/phosphor/${path}`,
    import.meta.resolve(`./node_modules/@phosphor-icons/web/src/${path}`),
  );

  site.add(`styles/vendor/phosphor/${path}`);
}

phosphor("fill/style.css");
phosphor("fill/Phosphor-Fill.svg");
phosphor("fill/Phosphor-Fill.ttf");
phosphor("fill/Phosphor-Fill.woff");
phosphor("fill/Phosphor-Fill.woff2");
phosphor("bold/style.css");
phosphor("bold/Phosphor-Bold.svg");
phosphor("bold/Phosphor-Bold.ttf");
phosphor("bold/Phosphor-Bold.woff");
phosphor("bold/Phosphor-Bold.woff2");

// MISC

site.add([".html"]);
site.add([".json"]);
site.use(sourceMaps());

// *.inline.js files are inlined into their companion HTML at build/serve time.
// Exclude them from the regular build so esbuild doesn't try to bundle them.
site.ignore((p) => p.endsWith(".inline.js"));

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

// MIDDLEWARE

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
  const isFacetHtml = pathname.endsWith(".html");
  const response = await next(request);

  if (!isFacetHtml || !response.headers.get("content-type")?.includes("html")) {
    return response;
  }

  let content = await response.text();
  content = await inlineScriptSrc(
    content,
    path.join("./src", path.dirname(pathname)),
  );

  const headers = new Headers(response.headers);
  headers.set("content-type", "text/plain; charset=utf-8");
  return new Response(content, {
    status: response.status,
    statusText: response.statusText,
    headers,
  });
}

const SCRIPT_SRC_RE =
  /<script type="module" src="\.\/([^"]+\.inline\.js)"><\/script>/;

async function inlineScriptSrc(content: string, dir: string): Promise<string> {
  const match = SCRIPT_SRC_RE.exec(content);
  if (!match) return content;

  const jsPath = path.join(dir, match[1]);
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

    const srcDir = path.dirname(f.path).replace(/^dist\//, "src/");
    const jsPath = path.join(srcDir, match[1]);

    try {
      const newContent = htmlWithInlineJs({ content, jsPath, match: match[0] });
      Deno.writeTextFileSync(f.path, newContent);
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
