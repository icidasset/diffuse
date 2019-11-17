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

    require("tailwindcss-animations")(),
    require("tailwindcss-interaction-variants")(),
    require("tailwindcss-transforms")(),
    require("tailwindcss-transitions")(),

    // Add variant for `:focus:not(:active)`
    function({ addVariant, e }) {
      addVariant("inactive-focus", ({ modifySelectors, separator }) => {
        modifySelectors(({ className }) => {
          return `.${e(`fixate${separator}${className}`)}:focus:not(:active)`
        })
      })
    }

  ],

  variants: {

    backgroundColor: [ "focus", "hover", "inactive-focus", "responsive" ],
    borderColor: [ "focus", "hover", "inactive-focus", "responsive" ],
    borderWidth: [ "first", "last" ],
    margin: [ "first", "last", "responsive" ],
    opacity: [ "focus", "hocus", "hover", "responsive" ],
    textColor: [ "focus", "hover", "inactive-focus", "responsive" ],

  },

  theme: {

    // Animations
    // ----------

    animations: {

      "fadeIn": {
        from: { opacity: "0" },
        to: { opacity: "1" },
      }

    },

    animationDelay: {
      "50ms": "50ms"
    },


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

      "background": "rgb(2, 7, 14)",
      "base01-15": "rgba(63, 63, 63, 0.15)",
      "base01-55": "rgba(63, 63, 63, 0.55)",
      "black_05": "rgba(0, 0, 0, 0.05)",
      "black_50": "rgba(0, 0, 0, 0.5)",
      "inherit": "inherit",
      "subtle": "rgb(238, 238, 238)",
      "white-60": "rgba(255, 255, 255, 0.6)",
      "white-90": "rgba(255, 255, 255, 0.9)",
    },


    // Extensions
    // ----------

    extend: {

      fontSize: {
        "0": 0
      },

      lineHeight: {
        "0": 0
      },

      screens: {
        "dark": { "raw": "(prefers-color-scheme: dark)" }
      },

      spacing: {
        "1/2": "50%",
        "1/3": "33.333333%",
        "2/3": "66.666667%",
        "1/4": "25%",
        "2/4": "50%",
        "3/4": "75%",
        "1/5": "20%",
        "2/5": "40%",
        "3/5": "60%",
        "4/5": "80%",
        "1/6": "16.666667%",
        "2/6": "33.333333%",
        "3/6": "50%",
        "4/6": "66.666667%",
        "5/6": "83.333333%",
        "1/7": "14.28571429%",
        "1/12": "8.333333%",
        "2/12": "16.666667%",
        "3/12": "25%",
        "4/12": "33.333333%",
        "5/12": "41.666667%",
        "6/12": "50%",
        "7/12": "58.333333%",
        "8/12": "66.666667%",
        "9/12": "75%",
        "10/12": "83.333333%",
        "11/12": "91.666667%",
      }

    },


    // Inset
    // -----

    inset: {
      "0": 0,
      "1/2": "50%",
      "full": "100%",
      "-px": "-1px"
    },


    // Min & Max
    // ---------

    maxWidth : {
      ...defaultTheme.maxWidth,

      "insulation": "107.5vh"
    },

    minWidth: {
      ...defaultTheme.maxWidth,

      "tiny": "13.125rem"
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

  }

}
