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
            |> insertVersion (de !~> "timestamp")
            |> write "../build"


list :: [Char] -> IO Dictionary
list pattern =
    Shikensu.listRelativeF "./src" [pattern] >>= Shikensu.read



-- SEQUENCES


data Sequence
    = Favicons
    | Fonts
    | Hosting
    | Html
    | Images
    | Manifests
    -- About Pages
    | AboutCss
    | AboutPages


sequences :: IO [( Sequence, Dictionary )]
sequences = lsequence
    [ ( Favicons,       list "Static/Favicons/**/*.*"   )
    , ( Fonts,          list "Static/Fonts/**/*.*"      )
    , ( Hosting,        list "Static/Hosting/**/*"      )
    , ( Html,           list "Static/Html/**/*.html"    )
    , ( Images,         list "Static/Images/**/*.*"     )
    , ( Manifests,      list "Static/Manifests/**/*.*"  )

    -- About Pages
    , ( AboutPages,     list "Static/About/**/*.md"    )
    , ( AboutCss,       list "Static/About/**/*.css"   )
    ]



-- FLOWS


flow :: Dependencies -> (Sequence, Dictionary) -> Dictionary
flow _ (Html, dict) =
    rename "Application.html" "index.html" dict


flow _ (Favicons, dict)       = dict
flow _ (Fonts, dict)          = prefixDirname "fonts/" dict
flow _ (Hosting, dict)        = dict
flow _ (Images, dict)         = prefixDirname "images/" dict


{-| Manifests -}
flow _ (Manifests, dict) =
    dict
        |> clone "manifest.json" "site.webmanifest"
        |> rename "Nextcloud/appinfo.xml" "appinfo/info.xml"


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
