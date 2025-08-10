import { defineCollection } from "astro:content";
import { glob } from "astro/loaders";

const manifests = defineCollection({
  loader: glob({ pattern: "**/_manifest.json", base: "./src/pages" }),
});

export const collections = { manifests };
