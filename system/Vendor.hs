module Main where

import Flow
import Shikensu
import Shikensu.Types
import Shikensu.Contrib
import Shikensu.Contrib.IO as Shikensu
import Shikensu.Utilities (mapIO)
import System.Process (readProcess)

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- | (• ◡•)| (❍ᴥ❍ʋ)


main :: IO Dictionary
main =
    Shikensu.listRelative [ "src/Vendor/**/*.js" ] "./"
        >>= mapIO browserify
        >>= return . prefixDirname "vendor"
        >>= write "./build"



-- Browserify


browserify :: Definition -> IO Definition
browserify def =
    readProcess "./node_modules/.bin/browserify" [ (workspacePath def) ] ""
        |> fmap (Text.pack)
        |> fmap (Text.encodeUtf8)
        |> fmap (\c -> def { content = Just c })
