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
      nodeModulesPolyfillPlugin(),
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

// MISC

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
