port module Stylesheets exposing (..)

import Css
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Styles


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    let
        css =
            Css.File.compile [ Css.stylesheet Styles.styles ]

        cssWithKeyframes =
            { css | css = String.join "\n" [ css.css, Styles.keyframes ] }
    in
        Css.File.toFileStructure
            [ ( "application.css", cssWithKeyframes ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
