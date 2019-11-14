const fs = require("fs")


// Table
// -----

const table = JSON.parse(fs.readFileSync(
  "build/css-table.json",
  { encoding: "utf-8" }
))



// Extractor
// ---------

class PurgeFromElm {
  static extract(content) {
    const results = content.match(/ (C\.\w+)/g) || []
    const classNames = results.reduce((acc, r) => {
      const key = r.replace(/^ C\./, "")
      const entry = table[key]
      return entry ? acc.concat([ entry ]) : acc
    }, [])

    classNames ? console.log(classNames) : null
    return classNames
  }
}



// Config
// ------

module.exports = {
  // defaultExtractor: content => content.match(/[\w-/:]+(?<!:)/g) || [],

  content: [ "src/Applications/**/*.elm" ],
  css: [ "build/application.css" ],

  extractors: [
    {
      extractor: PurgeFromElm,
      extensions: ["elm"]
    }
  ]
}
