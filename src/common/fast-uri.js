// ESM shim for fast-uri.
//
// Deno's npm:CJS interop puts `module.exports` on the `default` export but
// does not hoist properties to namespace members — so `import * as URI from
// "fast-uri"` gives `URI.parse === undefined` in Deno.  In esbuild (browser
// build) the hoisting works, but we use this shim so both runtimes see the
// same named-export interface.
//
// Mapped via `deno.jsonc` → `"fast-uri": "./src/common/fast-uri.js"`.

import fastUri from "npm:fast-uri@^3.1.0";

export const parse = fastUri.parse;
export const serialize = fastUri.serialize;

export default fastUri;
