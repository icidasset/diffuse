module Main where

import Flow
import Shikensu
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities (lsequence)

import qualified Control.Monad as Monad (join)
import qualified Data.List as List (concatMap)


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    sequences
        |> fmap (List.concatMap flow)
        |> fmap (write "./build")
        |> Monad.join


process :: [String] -> IO Dictionary
process patterns =
    Shikensu.listRelativeF "./" patterns >>= Shikensu.read



-- Sequences


sequences :: IO [( String, Dictionary )]
sequences =
    lsequence
        [ ( "pages",        process ["src/Static/Html/**/*.html"]       )
        , ( "images",       process ["src/Static/Images/**/*.*"]        )
        , ( "favicons",     process ["src/Static/Favicons/**/*.*"]      )
        , ( "fonts",        process ["src/Static/Fonts/**/*.*"]         )
        , ( "blockstack",   process ["src/Static/Blockstack/**/*"]      )
        , ( "js",           process ["src/Js/**/*.js"]                  )
        ]


flow :: (String, Dictionary) -> Dictionary
flow ("pages", dict) =
    dict
        |> rename "Proxy.html" "200.html"
        |> clone "200.html" "index.html"


flow ("images", dict) = prefixDirname "images/" dict
flow ("favicons", dict) = prefixDirname "favicons/" dict
flow ("fonts", dict) = prefixDirname "fonts/" dict
flow ("blockstack", dict) = dict
flow ("js", dict) = dict
