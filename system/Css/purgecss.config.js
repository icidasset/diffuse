module.exports = {
  content: [ "build/**/*.html" ],
  css: [ "build/stylesheet.css" ],
  defaultExtractor: content => content.match(/[\w-/:]+(?<!:)/g) || [],
  whitelistPatterns: []
}
