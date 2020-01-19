const fs = require("fs")


// Table
// -----

const table = JSON.parse(fs.readFileSync(
  "tmp/css-table.json",
  { flag: "a+", encoding: "utf-8" }
) || "{}")



// Extractor
// ---------

function purgeFromElm(elmCode) {
  const results = elmCode.match(/ (C\.\w+)/g) || []
  const classNames = results.reduce((acc, r) => {
    const key = r.replace(/^ C\./, "")
    const entry = table[key]
    return entry ? acc.concat([ entry ]) : acc
  }, [])

  return classNames
    .concat([ "button", "input", "select", "textarea" ])
}



// Config
// ------

module.exports = {
  content: [ "src/Applications/**/*.elm", "src/Library/Notifications.elm", "build/index.html" ],
  css: [ "build/application.css" ],

  extractors: [
    {
      extractor: purgeFromElm,
      extensions: [ "elm" ]
    },
    {
      extractor: content => content.match(/[\w-/:]+(?<!:)/g) || [],
      extensions: [ "html" ]
    }
  ]
}
