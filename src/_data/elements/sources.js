import { walk } from "@std/fs/walk";

const srcDir = new URL("../../", import.meta.url).pathname;
const sources = {};

for await (
  const entry of walk(srcDir + "components", { match: [/element\.js$/] })
) {
  const content = await Deno.readTextFile(entry.path);
  const key = entry.path.slice(srcDir.length);
  sources[key] = content;
}

export default sources;
