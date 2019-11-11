const fs = require("fs")
const postcss = require("postcss")


// Elm
// ---

const elmCssClasses = postcss.plugin("elm-css-classes", (_opts) => (root, result) => {

  const functions = []
  let lastCls

  root.walkRules(rule => {
    if (!rule.selector.startsWith(".")) return;
    if (rule.selector.includes(" ")) return;

    const cls = rule
      .selector
      .replace(/^\./, "")
      .replace(/\\/g, "")
      .replace("::placeholder", "")
      .replace(
        /\:(responsive|group-hover|focus-within|first|last|odd|even|hover|focus|active|visited|disabled)$/,
        ""
      )

    const elmVariable = cls
      .replace(/:/g, "__")
      .replace(/__-/g, "__neg_")
      .replace(/^-/g, "neg_")
      .replace(/-/g, "_")
      .replace(/\//g, "_div_")

    if (cls === lastCls) return;
    lastCls = cls

    functions.push(
      `{-| This represents the \`.${cls}\` class. -}\n` +
      `${elmVariable} : String\n` +
      `${elmVariable} = "${cls}"\n`
    )
  })

  const header = "module Css.Classes exposing (..)\n\n"
  const contents = header + functions.join("\n\n")

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
