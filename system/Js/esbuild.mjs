import esbuild from "esbuild"
import { wasmLoader } from "esbuild-plugin-wasm"

import parseArgv from "tiny-parse-argv"

const args = parseArgv(process.argv.slice(2), {
  string: ["alias", "define", "inject"],
})

esbuild.build({
  alias: {...obj("alias") },
  bundle: true,
  define: obj("define"),
  entryPoints: [args._[0]],
  format: "esm",
  inject: arr("inject"),
  minify: args.minify || false,
  outdir: args.outdir || undefined,
  outfile: args.outfile || undefined,
  plugins: [wasmLoader()],
  splitting: args.splitting || false,
  target: "esnext",
})

function arr(name) {
  return Object.entries(args)
    .filter(([k, v]) => {
      if (!k.includes(":")) return false
      return k.split(":")[0] == name
    })
    .map(([k, v]) => {
      return k.split(":").slice(1).join(":") + v
    })
}

function obj(name) {
  const entries = Object.entries(args)
    .filter(([k, v]) => {
      if (!k.includes(":")) return false
      return k.split(":")[0] == name
    })
    .map(([k, v]) => {
      return [k.split(":").slice(1).join(":"), v.toString()]
    })

  return Object.fromEntries(entries)
}
