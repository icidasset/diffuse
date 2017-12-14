port module Stylesheets exposing (..)

import Css
import Css.File exposing (CssFileStructure, CssCompilerProgram)
import Css.Normalize
import StylesOld


-- Ports


port files : CssFileStructure -> Cmd msg



-- Content


fileStructure : CssFileStructure
fileStructure =
    let
        css =
            Css.File.compile [ Css.Normalize.css, Css.stylesheet StylesOld.styles ]

        cssWithKeyframes =
            { css | css = String.concat [ css.css, StylesOld.keyframes ] }
    in
        Css.File.toFileStructure
            [ ( "application.css", cssWithKeyframes ) ]



-- Program


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
