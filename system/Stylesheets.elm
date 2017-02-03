port module Stylesheets exposing (..)

import Css
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Styles


-- Ports


port files : CssFileStructure -> Cmd msg



-- Content


fileStructure : CssFileStructure
fileStructure =
    let
        css =
            Css.File.compile [ Css.stylesheet Styles.styles ]

        cssWithKeyframes =
            { css | css = String.concat [ css.css, Styles.keyframes ] }
    in
        Css.File.toFileStructure
            [ ( "application.css", cssWithKeyframes ) ]



-- Program


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
