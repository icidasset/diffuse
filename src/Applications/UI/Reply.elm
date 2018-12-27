module UI.Reply exposing (Reply(..))

import Sources exposing (Source)
import UI.Page exposing (Page)



-- 🌳


type Reply
    = AddSourceToCollection Source
    | Chill
    | GoToPage Page
    | SaveEnclosedUserData
    | SaveHypaethralUserData
