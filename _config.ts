import lume from "lume/mod.ts";

import esbuild from "lume/plugins/esbuild.ts";
import postcss from "lume/plugins/postcss.ts";
import sourceMaps from "lume/plugins/source_maps.ts";

import * as path from "@std/path";
import { ensureDirSync } from "@std/fs/ensure-dir";
import { walkSync } from "@std/fs/walk";

const site = lume({
  src: "./src",
});

export default site;

// JS

site.use(esbuild({
  extensions: [".js"],
  options: {
    bundle: true,
    minify: false,
    // outExtension: { ".js": ".min.js" },
    splitting: true,
  },
}));

site.add([".js"]);

// CSS

site.use(postcss({ includes: false }));
site.add([".css"]);

// BINARY ASSETS

site.add("/favicons");
site.add("/fonts");
site.add("/images");

// MISC

site.use(sourceMaps());

// SCRIPTS

site.script("copy-type-defs", () => {
  for (
    const f of walkSync(
      "./src/",
      { includeDirs: false, exts: [".d.ts"] },
    )
  ) {
    const dest = "_site/" + f.path.replace(/^src\//, "");
    const dir = path.dirname(dest);
    ensureDirSync(dir);
    Deno.copyFileSync(f.path, dest);
  }
});

// site.addEventListener("afterBuild", () => {
//   site.run("copy-type-defs");
// });
