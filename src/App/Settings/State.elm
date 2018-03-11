module Settings.State exposing (..)

import List.Extra as List
import Response.Ext exposing (do)
import Settings.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { chosenBackdrop = Nothing
    , fadeInLastBackdrop = True
    , loadedBackdrops = []
    }


defaultBackdrop : String
defaultBackdrop =
    "7.jpg"



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetChosenBackdrop filename ->
            (!)
                { model | chosenBackdrop = Just filename }
                [ do TopLevel.DebounceStoreUserData ]

        SetDefaultBackdropIfNecessary ->
            case model.chosenBackdrop of
                Just _ ->
                    (!) model []

                Nothing ->
                    (!) { model | chosenBackdrop = Just defaultBackdrop } []

        SetLoadedBackdrop filename ->
            (!)
                { model
                    | fadeInLastBackdrop = True
                    , loadedBackdrops = loadBackdrop model.loadedBackdrops filename
                }
                []



-- 🔥  ~  Loaded backdrop


loadBackdrop : List String -> String -> List String
loadBackdrop state backdrop =
    case List.last state of
        Just lastBackdrop ->
            if backdrop /= lastBackdrop then
                state ++ [ backdrop ]
            else
                state

        Nothing ->
            [ backdrop ]
