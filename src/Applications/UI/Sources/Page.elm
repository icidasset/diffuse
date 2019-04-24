module UI.Sources.Page exposing (Page(..))

import Sources exposing (Service)



-- 🌳


type Page
    = Index
    | New
    | NewThroughRedirect Service { codeOrToken : Maybe String, state : Maybe String }
