module Main where

import Flow
import Protolude
import Shikensu
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    Shikensu.listRelative
        [ "node_modules/fast-text-encoding/text.min.js"
        , "node_modules/tachyons/css/tachyons.min.css"
        , "vendor/music-metadata.min.js"
        ]
        "./"
        >>= read
        >>= flow
        >>= write "./build"



-- Flow


flow :: Dictionary -> IO Dictionary
flow =
       rename "text.min.js" "text-encoding.js"
    .> rename "tachyons.min.css" "tachyons.css"
    .> rename "music-metadata.min.js" "music-metadata.js"
    .> prefixDirname "vendor"
    .> return
