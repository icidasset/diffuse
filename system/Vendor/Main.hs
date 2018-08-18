module Main where

import Flow
import Protolude
import Shikensu
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    Shikensu.listRelative
        [ "src/Vendor/**/*.js"

        --
        , "node_modules/blockstack/dist/blockstack.js"
        , "node_modules/jsmediatags/dist/jsmediatags.min.js"
        , "node_modules/lunr/lunr.js"
        , "node_modules/pepjs/dist/pep.min.js"
        , "node_modules/remotestoragejs/release/remotestorage.js"
        , "node_modules/serviceworker-cache-polyfill/index.js"
        , "node_modules/text-encoding/lib/encoding.js"
        , "node_modules/tocca/Tocca.min.js"
        , "node_modules/x0popup/dist/x0popup.min.js"
        ]
        "./"
        >>= read
        >>= flow
        >>= write "./build"



-- Flow


flow :: Dictionary -> IO Dictionary
flow =
       rename "encoding.js" "text-encoding.js"
    .> rename "index.js" "service-cache.js"
    .> rename "jsmediatags.min.js" "jsmediatags.js"
    .> rename "pep.min.js" "pep.js"
    .> rename "Tocca.min.js" "tocca.js"
    .> rename "x0popup.min.js" "x0popup.js"
    .> prefixDirname "vendor"
    .> return
