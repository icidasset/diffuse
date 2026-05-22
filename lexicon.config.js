import { defineLexiconConfig } from "@atcute/lex-cli";

export default defineLexiconConfig({
  files: ["lexicons/**/*.json"],
  outdir: "src/definitions/",
});
