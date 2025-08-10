import { defineConfig } from "astro/config";
import { fileURLToPath } from "node:url";
import scope from "astro-scope";
import path from "node:path";
import tsconfigPaths from "vite-tsconfig-paths";
import wasm from "vite-plugin-wasm";

import purgecss from "astro-purgecss";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

export default defineConfig({
  integrations: [scope(), purgecss()],
  build: {
    inlineStylesheets: "never",
  },
  vite: {
    plugins: [tsconfigPaths(), wasm()],
    server: {
      hmr: false,
    },
    build: {
      target: "esnext",
    },
    resolve: {
      alias: {
        "@applets": path.resolve(__dirname, "./src/pages"),
        "@layouts": path.resolve(__dirname, "./src/layouts"),
        "@pages": path.resolve(__dirname, "./src/pages"),
        "@scripts": path.resolve(__dirname, "./src/scripts"),
        "@styles": path.resolve(__dirname, "./src/styles"),
        "@src": path.resolve(__dirname, "./src"),
        "@phosphor-icons": path.resolve(__dirname, "./node_modules/@phosphor-icons/web/src"),
      },
    },
    worker: {
      format: "es",
    },
  },
});
