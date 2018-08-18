module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
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
import qualified Data.Text.Encoding as Text (encodeUtf8)
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
    = Css
    | Favicons
    | Fonts
    | Hosting
    | Images
    | InfoCss
    | InfoPages
    | Javascript
    | Manifest
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
        , ( Css,            list "Static/Css/**/*.css"      )
        , ( Images,         list "Static/Images/**/*.*"     )
        , ( Favicons,       list "Static/Favicons/**/*.*"   )
        , ( Fonts,          list "Static/Fonts/**/*.*"      )
        , ( Hosting,        list "Static/Hosting/**/*"      )
        , ( Manifest,       list "Static/manifest.json"     )

          -- Js
        , ( Javascript,     list "Js/**/*.js"               )
        ]



-- Flows


flow :: Dependencies -> (Sequence, Dictionary) -> Dictionary
flow _ (Pages, dict) =
    dict
        |> rename "Proxy.html" "index.html"
        |> clone "index.html" "200.html"


flow _ (Css, dict) =
    dict
        |> rename "Proxy.css" "index.css"
        |> map lowerCasePath


flow _ (Manifest, dict) =
    dict


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
flow x (Javascript, dict) =
    dict
        |> map lowerCasePath
        |> rename "workers/service.js" "service-worker.js"
        |> insertVersion (x !~> "timestamp")


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
    infoLayout      <- Text.readFile "src/Static/Info/Layout.html"
    timestamp       <- fmap show unixTime :: IO Text

    return $ HashMap.fromList
        [ ("infoLayout", Aeson.toJSON infoLayout)
        , ("timestamp", Aeson.toJSON timestamp)
        ]



-- Insertions


insertTree :: Dictionary -> Dictionary
insertTree dict =
    let
        treeContent =
            dict
                |> List.map localPath
                |> Aeson.encode
                |> BSL.toStrict

        defs =
            case headMay dict of
                Just def ->
                    def
                        |> forkDefinition "tree.json"
                        |> setContent treeContent
                        |> wrap

                Nothing ->
                    []
    in
        dict <> defs


insertVersion :: Text -> Dictionary -> Dictionary
insertVersion version dict =
    let
        versionContent =
            Text.encodeUtf8 ("self.VERSION = \"" <> version <> "\";")

        defs =
            case headMay dict of
                Just def ->
                    def
                        |> forkDefinition "version.js"
                        |> setContent versionContent
                        |> wrap

                Nothing ->
                    []
    in
        dict <> defs



-- Utilities


lowerCasePath :: Definition -> Definition
lowerCasePath def =
    Shikensu.forkDefinition
        ( def
            |> localPath
            |> List.map Char.toLower
        )
        def


setContent :: ByteString -> Definition -> Definition
setContent theContent definition =
    definition { content = Just theContent }


unixTime :: IO Int
unixTime =
    fmap floor getPOSIXTime


wrap :: a -> [a]
wrap a =
    [a]
