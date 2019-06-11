module Main where

import Data.Time.Clock.POSIX (getPOSIXTime)
import Flow
import Protolude hiding (list)
import Renderers
import Shikensu hiding (list)
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap (fromList)
import qualified Data.List as List
import qualified Data.Text.Encoding as Text
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



-- SEQUENCES


data Sequence
    = Css
    | Favicons
    | Fonts
    | Hosting
    | Html
    | Images
    | Js
    | Manifest
    -- About Pages
    | AboutCss
    | AboutPages


sequences :: IO [( Sequence, Dictionary )]
sequences = lsequence
    [ ( Css,            list "Static/Css/**/*.css"      )
    , ( Favicons,       list "Static/Favicons/**/*.*"   )
    , ( Fonts,          list "Static/Fonts/**/*.*"      )
    , ( Hosting,        list "Static/Hosting/**/*"      )
    , ( Html,           list "Static/Html/**/*.html"    )
    , ( Images,         list "Static/Images/**/*.*"     )
    , ( Js,             list "Javascript/**/*.js"       )
    , ( Manifest,       list "Static/manifest.json"     )

    -- About Pages
    , ( AboutPages,      list "Static/About/**/*.md"    )
    , ( AboutCss,        list "Static/About/**/*.css"   )
    ]



-- FLOWS


flow :: Dependencies -> (Sequence, Dictionary) -> Dictionary
flow _ (Html, dict) =
    dict
        |> rename "Application.html" "200.html"
        |> clone "200.html" "index.html"


flow _ (Css, dict)            = dict |> map lowerCasePath
flow _ (Favicons, dict)       = dict
flow _ (Fonts, dict)          = prefixDirname "fonts/" dict
flow _ (Hosting, dict)        = dict
flow _ (Images, dict)         = prefixDirname "images/" dict
flow _ (Manifest, dict)       = dict


{-| Javascript -}
flow x (Js, dict) =
    dict
        |> map lowerCasePath
        |> rename "workers/service.js" "service-worker.js"
        |> insertVersion (x !~> "timestamp")


{-| About Pages -}
flow _ (AboutCss, dict) =
    dict
        |> map lowerCasePath
        |> prefixDirname "about/"

flow x (AboutPages, dict) =
    dict
        |> renderContent markdownRenderer
        |> renderContent (layoutRenderer $ x !~> "aboutLayout")
        |> rename "About.md" "index.html"
        |> prefixDirname "about/"



-- ADDITIONAL IO
-- FLOW DEPENDENCIES


type Dependencies = Aeson.Object


dependencies :: IO Dependencies
dependencies = do
    aboutLayout     <- Text.readFile "src/Static/About/Layout.html"
    timestamp       <- fmap show unixTime :: IO Text

    return $ HashMap.fromList
        [ ( "aboutLayout", Aeson.toJSON aboutLayout )
        , ( "timestamp", Aeson.toJSON timestamp )
        ]


-- INSERT


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
                        |> wrap
                        |> setContent treeContent

                Nothing ->
                    []
    in
    dict <> defs


insertVersion :: Text -> Dictionary -> Dictionary
insertVersion version dict =
    let
        versionContent =
            Text.encodeUtf8 ("self.VERSION = \"" <> version <> "\"")

        defs =
            case headMay dict of
                Just def ->
                    def
                        |> forkDefinition "version.js"
                        |> wrap
                        |> setContent versionContent

                Nothing ->
                        []
    in
    dict <> defs


-- COMMON


lowerCasePath :: Definition -> Definition
lowerCasePath def =
    Shikensu.forkDefinition
        ( def
            |> localPath
            |> List.map Char.toLower
        )
        def


unixTime :: IO Int
unixTime =
    fmap floor getPOSIXTime


wrap :: a -> [a]
wrap a =
    [a]
