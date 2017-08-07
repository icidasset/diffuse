module Main where

import Flow
import Protolude
import Renderers
import Shikensu
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
        |> fmap (write "./build")
        |> Monad.join


process :: [[Char]] -> IO Dictionary
process patterns =
    Shikensu.listRelativeF "./" patterns >>= Shikensu.read



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
        [ ( Pages,          process ["src/Static/Html/**/*.html"]       )
        , ( Info,           process ["src/Static/Info/**/*.*"]          )

        , ( Images,         process ["src/Static/Images/**/*.*"]        )
        , ( Favicons,       process ["src/Static/Favicons/**/*.*"]      )
        , ( Fonts,          process ["src/Static/Fonts/**/*.*"]         )
        , ( Blockstack,     process ["src/Static/Blockstack/**/*"]      )
        , ( Javascript,     process ["src/Js/**/*.js"]                  )
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

        append =
            \a b -> List.concat [ a, b ]
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


flow (Images, dict) = prefixDirname "images/" dict
flow (Favicons, dict) = prefixDirname "favicons/" dict
flow (Fonts, dict) = prefixDirname "fonts/" dict
flow (Blockstack, dict) = dict
flow (Javascript, dict) = dict
