import { readTextFileSync } from "@std/fs/unstable-read-text-file";
import { writeTextFileSync } from "@std/fs/unstable-write-text-file";

/**
 * JSR forbids "modifying global types" in published packages. The lex-cli
 * generated definition files register each lexicon's schema into the global
 * `Records` map by augmenting the `@atcute/lexicons/ambient` module:
 *
 *   declare module "@atcute/lexicons/ambient" {
 *     interface Records { "sh.diffuse.…": mainSchema; }
 *   }
 *
 * Diffuse doesn't consume that global registry (its components import the
 * concrete generated types directly), so we strip both the augmentation block
 * and the now-unused side-effect type import. This runs as a post-gen step so
 * regenerating the definitions stays JSR-publishable.
 */

const AMBIENT_IMPORT = 'import type {} from "@atcute/lexicons/ambient";';

const AMBIENT_BLOCK =
  /declare module "@atcute\/lexicons\/ambient" \{\s*\n\s*interface Records \{\s*\n\s*"[^"]+": [a-zA-Z]+\w*;\s*\n\s*\}\s*\n\}/g;

function* tsFiles(dir: string): Generator<string> {
  for (const entry of Deno.readDirSync(dir)) {
    const path = `${dir}/${entry.name}`;
    if (entry.isDirectory) {
      yield* tsFiles(path);
    } else if (entry.isFile && entry.name.endsWith(".ts")) {
      yield path;
    }
  }
}

function strip(path: string) {
  let text = readTextFileSync(path);
  const before = text;

  text = text.replace(AMBIENT_BLOCK, "");
  text = text.replace(`${AMBIENT_IMPORT}\n`, "");

  // Avoid touching files that never had the augmentation.
  if (text !== before) {
    writeTextFileSync(path, text);
  }
}

for (const path of tsFiles("./src/definitions/types")) {
  strip(path);
}