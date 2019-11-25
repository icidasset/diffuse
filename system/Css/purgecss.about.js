module.exports = {
  defaultExtractor: content => content.match(/[\w-/:]+(?<!:)/g) || [],
  content: [ "build/about/**/*.html" ],
  css: [ "build/about.css" ]
}
