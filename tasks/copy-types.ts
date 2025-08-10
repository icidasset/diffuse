import { walk } from "jsr:@std/fs@^1.0";

// Copy .d.ts files to dist dir
const it = walk("./src/pages/", { exts: [".d.ts"] });

for await (const item of it) {
  const target = `dist/${item.path.replace("src/pages/", "")}`;
  const targetDir = target.split("/").slice(0, -1).join("/");

  Deno.mkdirSync(targetDir, { recursive: true });
  Deno.copyFileSync(item.path, target);
}
