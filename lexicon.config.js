import { defineLexiconConfig } from "@atcute/lex-cli";

export default defineLexiconConfig({
  files: ["src/definitions/**/*.json"],
  outdir: "src/definitions/",
});
