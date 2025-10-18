import lume from "lume/mod.ts";

import esbuild from "lume/plugins/esbuild.ts";
import postcss from "lume/plugins/postcss.ts";

const site = lume({
  src: "./src",
});

// JS

site.use(esbuild({
  options: {
    bundle: true,
    minify: false,
    splitting: true,
  },
}));

site.add([".js", ".d.ts"]);

export default site;

// CSS

site.use(postcss({ includes: false }));

site.add([".css"]);

// BINARY ASSETS

site.add("/favicons");
site.add("/fonts");
site.add("/images");
