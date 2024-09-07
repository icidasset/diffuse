module UI.Sources.Page exposing (Page(..))

import Sources exposing (Service)



-- 🌳


type Page
    = Index
    | Edit String
    | New
    | NewOnboarding
    | NewThroughRedirect Service { codeOrToken : Maybe String, state : Maybe String }
    | Rename String
