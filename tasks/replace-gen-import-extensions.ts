import { readTextFileSync } from "@std/fs/unstable-read-text-file";
import { writeTextFileSync } from "@std/fs/unstable-write-text-file";

const PATH = "./src/definitions/index.ts";

const text = readTextFileSync(PATH);
const withTsImports = text.replaceAll(/\.js";/g, '.ts";');

writeTextFileSync(PATH, withTsImports);
