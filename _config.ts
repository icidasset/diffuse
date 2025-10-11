import lume from "lume/mod.ts";
import esbuild from "lume/plugins/esbuild.ts";

const site = lume({
  src: "./src",
});

// JS

site.use(esbuild({
  options: {
    bundle: true,
    splitting: true,
  },
}));

site.add([".js", ".d.ts"]);

export default site;
