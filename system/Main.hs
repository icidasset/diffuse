module Main where

import Control.Monad as Monad ((>=>), join)
import Flow
import Shikensu
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities (lsequence)
import System.Directory (canonicalizePath)

import qualified Data.List as List (concatMap)


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    sequences
        |> fmap (List.concatMap flow)
        |> fmap (write "./build")
        |> Monad.join



-- Sequences


sequences :: IO [( String, Dictionary )]
sequences =
    let
        process =
            \p -> canonicalizePath "./" >>= list p
    in
        lsequence
            [ ( "pages",  process ["src/Static/Html/**/*.html"] >>= Shikensu.read )
            , ( "images", process ["src/Static/Images/**/*.*"]  >>= Shikensu.read )
            , ( "js",     process ["src/Js/**/*.js"]            >>= Shikensu.read )
            ]


flow :: (String, Dictionary) -> Dictionary
flow ("pages", dict) =
    dict
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"


flow ("images", dict) = prefixDirname "images/" dict
flow ("js", dict) = dict
