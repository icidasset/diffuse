import { readTextFileSync } from "@std/fs/unstable-read-text-file";
import { writeTextFileSync } from "@std/fs/unstable-write-text-file";

function replace(path: string) {
  const text = readTextFileSync(path);
  const withTsImports = text.replaceAll(/\.js";/g, '.ts";');

  writeTextFileSync(path, withTsImports);
}

replace("./src/definitions/index.ts");
replace("./src/definitions/types/sh/diffuse/output/playlistItemBundle.ts");
replace("./src/definitions/types/sh/diffuse/output/trackBundle.ts");
