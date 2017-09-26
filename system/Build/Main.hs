module Main where

import Flow
import Protolude hiding (list)
import Renderers
import Shikensu hiding (list)
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities

import qualified Control.Monad as Monad (join)
import qualified Data.Aeson as Aeson (Object, Value, toJSON)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap (fromList)
import qualified Data.List as List
import qualified Data.Text.IO as Text


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    do
        de <- dependencies
        se <- sequences

        -- Execute flows
        -- & reduce to a single dictionary
        let dictionary = List.concatMap (flow de) se

        -- Write to disk
        write "../build" dictionary


list :: [Char] -> IO Dictionary
list pattern =
    Shikensu.listRelativeF "./src" [pattern] >>= Shikensu.read



-- Sequences


data Sequence
    = Blockstack
    | Favicons
    | Fonts
    | Images
    | InfoCss
    | InfoPages
    | Javascript
    | Pages


sequences :: IO [( Sequence, Dictionary )]
sequences =
    lsequence
        [ -- Pages
          ( Pages,          list "Static/Html/**/*.html"    )

          -- Info / About
        , ( InfoPages,      list "Static/Info/**/*.md"      )
        , ( InfoCss,        list "Static/Info/**/*.css"     )

          -- Assets
        , ( Images,         list "Static/Images/**/*.*"     )
        , ( Favicons,       list "Static/Favicons/**/*.*"   )
        , ( Fonts,          list "Static/Fonts/**/*.*"      )
        , ( Blockstack,     list "Static/Blockstack/**/*"   )

          -- Js
        , ( Javascript,     list "Js/**/*.js"               )
        ]



-- Flows


flow :: Dependencies -> (Sequence, Dictionary) -> Dictionary
flow _ (Pages, dict) =
    dict
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"


flow deps (InfoPages, dict) =
    dict
        |> renderContent markdownRenderer
        |> renderContent (layoutRenderer $ deps !~> "infoLayout")
        |> rename "Info.md" "index.html"
        |> prefixDirname "about/"


flow _ (InfoCss, dict) =
    dict
        |> rename "Info.css" "about.css"
        |> prefixDirname "about/"


flow _ (Images, dict)         = prefixDirname "images/" dict
flow _ (Favicons, dict)       = prefixDirname "favicons/" dict
flow _ (Fonts, dict)          = prefixDirname "fonts/" dict
flow _ (Blockstack, dict)     = dict
flow _ (Javascript, dict)     = List.map lowerCasePath dict



-- Additional IO
-- Flow dependencies


type Dependencies = Aeson.Object


dependencies :: IO Dependencies
dependencies = do
    infoLayout <- Text.readFile "src/Static/Info/Layout.html"

    return $ HashMap.fromList
        [ ("infoLayout", Aeson.toJSON infoLayout)
        ]



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
