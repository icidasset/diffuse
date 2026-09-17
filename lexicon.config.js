import { defineLexiconConfig } from "@atcute/lex-cli";

export default defineLexiconConfig({
  generate: {
    files: ["lexicons/output/*.json"],
    outdir: "src/definitions/",
  },
});
