module Main where

import Control.Monad.IO.Class (liftIO)
import Flow
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header (hAccept)
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors
import Protolude
import System.Directory (doesFileExist)

import qualified Data.Binary.Builder as Builder (empty, putStringUtf8)
import qualified Data.List as List (elem, filter, head, last)
import qualified Data.Text as Text (isInfixOf, isSuffixOf, pack)
import qualified Data.Tuple as Tuple (snd)


main :: IO ()
main = run 5000 (simpleCors app)


app :: Application
app = staticApp $ config { ss404Handler = Just notFound }


config :: StaticSettings
config = defaultFileServerSettings "./build"


{-| Emulate surge.sh.

Present `200.html` if we have a request for a html file that doesn't exist.
And if the `200.html` file doesn't exist, render `404.html`.

The server decides it's a request for a html file
if one of the following conditions is true:

- Does the requested path have NO extension?
- Is the extension `.html`?

This is by all means not perfect, but it does the job.
Let me know if you know how to improve this.

-}
notFound :: Application
notFound request sendResponse =
    let
        lastPiece =
            List.last (pathInfo request)

        wantsHtml =
            (lastPiece == "") ||
            (Text.isInfixOf "." lastPiece == False) ||
            (Text.isSuffixOf ".html" lastPiece)

        existance =
            sequence
                [ doesFileExist "./build/200.html"
                , doesFileExist "./build/404.html"
                ]
            |> liftIO

        response =
            \bools ->
                if wantsHtml then
                    if List.head bools then
                        responseFile
                            status200
                            [( "Content-Type", "text/html" )]
                            "./build/200.html"
                            Nothing

                    else if List.last bools then
                        responseFile
                            status404
                            [( "Content-Type", "text/html" )]
                            "./build/404.html"
                            Nothing

                    else
                        responseBuilder
                            status404
                            []
                            (Builder.putStringUtf8 "Page not found.")

                else
                    responseBuilder
                        status404
                        []
                        Builder.empty
    in
        existance >>= sendResponse . response
