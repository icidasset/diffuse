module Main where

import Flow
import Protolude hiding (list)
import Renderers
import Shikensu hiding (list)
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities

import qualified Control.Monad as Monad (join)
import qualified Data.Aeson as Aeson (Object, Value, encode, toJSON)
import qualified Data.ByteString.Lazy as BSL (toStrict)
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

        -- Make a file tree
        -- and then write to disk
        dictionary
            |> insertTree
            |> write "../build"


list :: [Char] -> IO Dictionary
list pattern =
    Shikensu.listRelativeF "./src" [pattern] >>= Shikensu.read



-- Sequences


data Sequence
    = CachePolyfill
    | Favicons
    | Fonts
    | Hosting
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
        , ( Hosting,        list "Static/Hosting/**/*"      )

          -- Js
        , ( Javascript,     list "Js/**/*.js"               )
        , ( CachePolyfill,  list serviceCachePolyfill       )
        ]


{-| Path to the Service-Worker Cache polyfill.
-}
serviceCachePolyfill :: [Char]
serviceCachePolyfill =
    "../node_modules/serviceworker-cache-polyfill/index.js"



-- Flows


flow :: Dependencies -> (Sequence, Dictionary) -> Dictionary
flow _ (Pages, dict) =
    dict
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"


{-| Info -}
flow x (InfoPages, dict) =
    dict
        |> renderContent markdownRenderer
        |> renderContent (layoutRenderer $ x !~> "infoLayout")
        |> rename "Info.md" "index.html"
        |> prefixDirname "about/"


flow _ (InfoCss, dict) =
    dict
        |> rename "Info.css" "about.css"
        |> prefixDirname "about/"


{-| Javascript -}
flow _ (Javascript, dict) =
    dict
        |> List.map lowerCasePath
        |> rename "workers/service.js" "service-worker.js"


flow _ (CachePolyfill, dict) =
    dict
        |> rename "index.js" "service-cache.js"
        |> prefixDirname "vendor/"


{-| Other -}
flow _ (Images, dict)         = prefixDirname "images/" dict
flow _ (Fonts, dict)          = prefixDirname "fonts/" dict
flow _ (Favicons, dict)       = dict
flow _ (Hosting, dict)        = dict



-- Additional IO
-- Flow dependencies


type Dependencies = Aeson.Object


dependencies :: IO Dependencies
dependencies = do
    infoLayout <- Text.readFile "src/Static/Info/Layout.html"

    return $ HashMap.fromList
        [ ("infoLayout", Aeson.toJSON infoLayout)
        ]



-- Tree


excludeFromTree :: [[Char]]
excludeFromTree =
    [ "_headers"
    , "_redirects"
    , "CORS"
    ]


insertTree :: Dictionary -> Dictionary
insertTree dict =
    let
        treeFilter =
            \l -> List.find ((==) l) excludeFromTree == Nothing

        treeContent =
            dict
            |> List.map localPath
            |> List.filter treeFilter
            |> Aeson.encode
            |> BSL.toStrict
            |> Just

        treeDef =
            dict
            |> List.head
            |> forkDefinition "tree.json"
            |> (\def -> def { content = treeContent })
    in
        dict <> [treeDef]



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
