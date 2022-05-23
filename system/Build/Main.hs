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
import qualified Data.Aeson.KeyMap as KeyMap (fromList)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.IO as Text


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO ()
main =
    do
        de <- dependencies
        se <- sequences

        -- Execute flows
        -- & reduce to a single dictionary
        let dictionary = List.concatMap (flow de) se

        -- Write everything to disk
        write "../build" dictionary

        -- Make a file tree
        build <- list "../build/**/*.*"

        build
            |> makeTree
            |> write "../build"

        -- Inject version timestamp
        insertVersion (de !~> "timestamp") build

        -- Fin
        return ()


list :: [Char] -> IO Dictionary
list pattern =
    Shikensu.listRelativeF "./src" [pattern] >>= Shikensu.read



-- SEQUENCES


data Sequence
    = AboutPages
    | Favicons
    | Fonts
    | Hosting
    | Html
    | Images
    | Manifests


sequences :: IO [( Sequence, Dictionary )]
sequences = lsequence
    [ ( AboutPages,     list "Static/About/**/*.md"     )
    , ( Favicons,       list "Static/Favicons/**/*.*"   )
    , ( Fonts,          list "Static/Fonts/**/*.*"      )
    , ( Hosting,        list "Static/Hosting/**/*"      )
    , ( Html,           list "Static/Html/**/*.html"    )
    , ( Images,         list "Static/Images/**/*.*"     )
    , ( Manifests,      list "Static/Manifests/**/*.*"  )
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
flow x (AboutPages, dict) =
    dict
        |> map lowerCasePath
        |> renameExt ".md" ".html"
        |> permalink "index"
        |> prefixDirname "about/"
        |> renderContent markdownRenderer
        |> renderContent (layoutRenderer $ x !~> "aboutLayout")



-- ADDITIONAL IO
-- FLOW DEPENDENCIES


type Dependencies = Aeson.Object


dependencies :: IO Dependencies
dependencies = do
    aboutLayout     <- Text.readFile "src/Static/About/Layout.html"
    timestamp       <- fmap show unixTime :: IO Text

    return $ KeyMap.fromList
        [ ( "aboutLayout", Aeson.toJSON aboutLayout )
        , ( "timestamp", Aeson.toJSON timestamp )
        ]



-- INSERT


makeTree :: Dictionary -> Dictionary
makeTree dict =
    let
        treeContent =
            dict
                |> List.map localPath
                |> List.filter (\p -> p /= "tree.json")
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
    defs


insertVersion :: Text -> Dictionary -> IO ()
insertVersion version dict = do
    let sw = List.filter
                (\def -> localPath def == "service-worker.js")
                dict

    case headMay sw of
        Just def ->
            def
                |> content
                |> fmap Text.decodeUtf8
                |> fmap (Text.replace "{{VERSION}}" version)
                |> fmap Text.encodeUtf8
                |> (\c -> def { content = c })
                |> writeDef "../build"
                |> fmap (\_ -> ())

        Nothing ->
            return ()



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
