module Renderers where

import Flow
import Protolude
import Shikensu

import qualified CMark
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- Layouts


layoutRenderer :: Text -> Definition -> Maybe ByteString
layoutRenderer layout def =
  let
    layoutWithoutVariables =
        Text.replace
          "{{pathToRoot}}"
          (Text.pack <| pathToRoot def)
          layout
  in
  content def
      |> fmap Text.decodeUtf8
      |> fmap (\text -> Text.replace "<placeholder />" text layoutWithoutVariables)
      |> fmap Text.encodeUtf8



-- Markdown


markdownRenderer :: Definition -> Maybe ByteString
markdownRenderer def =
  content def
    |> fmap Text.decodeUtf8
    |> fmap (CMark.commonmarkToHtml [ CMark.optSmart, CMark.optUnsafe ])
    |> fmap Text.encodeUtf8
