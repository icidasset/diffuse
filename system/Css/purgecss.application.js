const fs = require("fs")


// Table
// -----

const table = JSON.parse(fs.readFileSync(
  "build/css-table.json",
  { encoding: "utf-8", flag: "a+" }
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
