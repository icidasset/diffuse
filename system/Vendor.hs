module Main where

import Flow
import Shikensu
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities (mapIO)
import System.Process (readProcess)

import qualified Data.ByteString.Char8 as ByteString (pack)


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    Shikensu.listRelative vendorScripts "./"
        >>= mapIO browserify
        >>= return . prefixDirname "vendor"
        >>= write "./build"



-- List


vendorScripts :: [String]
vendorScripts =
    [ "src/Vendor/package.js"
    ]



-- Browserify


browserify :: Definition -> IO Definition
browserify def =
    readProcess "./node_modules/.bin/browserify" [ (workspacePath def) ] ""
        |> fmap (ByteString.pack)
        |> fmap (\c -> def { content = Just c })
