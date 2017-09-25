module Main where

import Flow
import Protolude hiding (list)
import Renderers
import Shikensu hiding (list)
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities (lsequence)

import qualified Control.Monad as Monad (join)
import qualified Data.Char as Char
import qualified Data.List as List


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    sequences
        |> fmap (List.concatMap flow)
        |> fmap (Shikensu.write "../build")
        |> Monad.join


list :: [Char] -> IO Dictionary
list pattern =
    Shikensu.listRelativeF "./src" [pattern] >>= Shikensu.read



-- Sequences


data Sequence
    = Blockstack
    | Favicons
    | Fonts
    | Images
    | Info
    | Javascript
    | Pages


sequences :: IO [( Sequence, Dictionary )]
sequences =
    lsequence
        [ ( Pages,          list "Static/Html/**/*.html"    )
        , ( Info,           list "Static/Info/**/*.*"       )

        , ( Images,         list "Static/Images/**/*.*"     )
        , ( Favicons,       list "Static/Favicons/**/*.*"   )
        , ( Fonts,          list "Static/Fonts/**/*.*"      )
        , ( Blockstack,     list "Static/Blockstack/**/*"   )
        , ( Javascript,     list "Js/**/*.js"               )
        ]



-- Flows


flow :: (Sequence, Dictionary) -> Dictionary
flow (Pages, dict) =
    dict
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"


flow (Info, dict) =
    let
        layout =
            List.find (\def -> basename def == "Layout") dict >>= content

        css =
            List.filter (\def -> extname def == ".css") dict
    in
        dict
            |> exclude "Layout.html"
            |> exclude "Info.css"
            |> renderContent markdownRenderer
            |> renderContent (layoutRenderer layout)
            |> append css
            |> rename "Info.md" "index.html"
            |> rename "Info.css" "about.css"
            |> prefixDirname "about/"


flow (Images, dict)         = prefixDirname "images/" dict
flow (Favicons, dict)       = prefixDirname "favicons/" dict
flow (Fonts, dict)          = prefixDirname "fonts/" dict
flow (Javascript, dict)     = List.map lowerCasePath dict
flow (Blockstack, dict)     = dict



-- Utilities


append :: Dictionary -> Dictionary -> Dictionary
append a b =
    List.concat [ a, b ]


lowerCasePath :: Definition -> Definition
lowerCasePath def =
    Shikensu.forkDefinition
        ( def
            |> localPath
            |> List.map Char.toLower
        )
        def
