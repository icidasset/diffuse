const defaultTheme = require("tailwindcss/defaultTheme")


// Colors
// ------

const colors = {
  base00: "rgb(45, 45, 45)",
  base01: "rgb(63, 63, 63)",
  base02: "rgb(79, 79, 79)",
  base03: "rgb(119, 119, 119)",
  base04: "rgb(140, 140, 140)",
  base05: "rgb(163, 163, 163)",
  base06: "rgb(186, 186, 186)",
  base07: "rgb(232, 232, 232)",
  base08: "rgb(239, 97, 85)",
  base09: "rgb(249, 155, 21)",
  base0a: "rgb(254, 196, 24)",
  base0b: "rgb(72, 182, 133)",
  base0c: "rgb(91, 196, 191)",
  base0d: "rgb(6, 182, 239)",
  base0e: "rgb(129, 91, 164)",
  base0f: "rgb(233, 107, 168)",

  accent: "rgb(231, 150, 128)"
}



// Config
// ------

module.exports = {

  plugins: [
    require("tailwindcss-transforms")(),
  ],

  theme: {

    // Fonts
    // -----

    fontFamily: {
      body: '"Source Sans Pro", sans-serif',
      display: '"Montserrat", Futura, "Trebuchet MS", Arial, sans-serif',
      mono: 'Hack, Consolas, Menlo, Monaco, "Andale Mono WT", "Andale Mono", "Lucida Console", "Lucida Sans Typewriter", "DejaVu Sans Mono", "Bitstream Vera Sans Mono", "Liberation Mono", "Nimbus Mono L", "Courier New", Courier, monospace'
    },


    // Colors
    // ------

    colors: {
      ...defaultTheme.colors,
      ...colors,

      background: "rgb(2, 7, 14)",
      inherit: "inherit",
    },


    // Extensions
    // ----------

    extend: {
      screens: {
        "dark": { "raw": "(prefers-color-scheme: dark)" }
      }
    },


    // Inset
    // -----

    inset: {
      "0": 0,
      "1/2": "50%",
      "full": "100%",
    },


    // Opacity
    // -------

    opacity: {
      '0': '0',
      '10': '.1',
      '20': '.2',
      '30': '.3',
      '40': '.4',
      '50': '.5',
      '60': '.6',
      '70': '.7',
      '80': '.8',
      '90': '.9',
      '100': '1'
    },


    // Transforms
    // ----------

    translate: {
      "1/2": "50%",
      "-1/2": "-50%",
      "put-on-top": ["-50%", "-100%"],
    },

    transformOrigin: {},
    perspectiveOrigin: {}

  },

  variants: {

    // Variants
    // --------

    margin: [ "first", "last", "responsive" ]

  }

}
