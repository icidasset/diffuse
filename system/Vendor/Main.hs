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
    .> prefixDirname "vendor"
    .> return
