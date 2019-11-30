const crypto = require("crypto")
const fs = require("fs")
const postcss = require("postcss")


// Elm
// ---
// This'll generate an Elm module with a function for each CSS class we have.
// It will also generate a "CSS table" with the "css_class <=> elm_function" relation.
// This "CSS table" makes it possible to only keep the CSS that's actually used.

const elmCssClasses = postcss.plugin("elm-css-classes", (_opts) => (root, result) => {

  if (result.opts.from.endsWith("/Css/About.css")) return;

  const functions = []
  const lookup = {}

  let lastCls

  root.walkRules(rule => {
    if (!rule.selector.startsWith(".")) return;
    if (rule.selector.includes(" ")) return;

    const cls = rule
      .selector
      .split(",")[0]
      .replace(/^\./, "")
      .replace(/\\/g, "")
      .replace("::placeholder", "")
      .replace(
        /\:(active|disabled|even|first-child|focus|focus-within|focus:not\(:active\)|group-hover|hover|last-child|nth-child\(odd\)|odd|responsive|visited)$/,
        ""
      )

    const elmVariable = cls
      .replace(/:/g, "__")
      .replace(/__-/g, "__minus_")
      .replace(/^-/g, "minus_")
      .replace(/-/g, "_")
      .replace(/\./g, "_")
      .replace(/\//g, "_div_")
      .replace(/_1_div_2$/, "_half")

    if (cls === lastCls) return;
    lastCls = cls

    const css = rule
      .toString()
      .replace(/\s+/g, " ")
      .replace(/(\w)\{/g, "$1 {")

    functions.push(
      `{-| This represents the \`.${cls}\` class.\n` +
      `\n    ${css}` +
      `\n-}\n` +
      `${elmVariable} : String\n` +
      `${elmVariable} = "${cls}"\n`
    )

    lookup[elmVariable] = cls
  })

  const header = "module Css.Classes exposing (..)\n\n"
  const contents = header + functions.join("\n\n")
  const table = JSON.stringify(lookup)
  const hash = crypto.createHash("sha1").update(table).digest("base64")
  const previousHash = fs.readFileSync("tmp/css-table.cache", { flag: "a+", encoding: "utf-8" })

  if (hash === previousHash) return;

  fs.writeFileSync("tmp/css-table.cache", hash)
  fs.writeFileSync("tmp/css-table.json", table)
  fs.writeFileSync("src/Library/Css/Classes.elm", contents)
})



// Config
// ------

module.exports = {
  plugins: [
    require("postcss-import"),
    require("tailwindcss")("system/Css/Tailwind.js"),

    elmCssClasses(),

    require("postcss-nesting"),
    require("autoprefixer"),
  ]
}
