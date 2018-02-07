module Settings.State exposing (..)

import Response.Ext exposing (do)
import Settings.Types exposing (..)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { chosenBackdrop = "7.jpg"
    , loadedBackdrop = { previous = Nothing, current = Nothing }
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetChosenBackdrop filename ->
            (!)
                { model | chosenBackdrop = filename }
                [ do TopLevel.DebounceStoreUserData ]

        SetLoadedBackdrop filename ->
            (!)
                { model | loadedBackdrop = loadBackdrop model.loadedBackdrop filename }
                []



-- 🔥  ~  Loaded backdrop


loadBackdrop : LoadedBackdrop -> String -> LoadedBackdrop
loadBackdrop state backdrop =
    let
        prev =
            case state.current of
                Just x ->
                    if x == backdrop then
                        state.previous
                    else
                        state.current

                Nothing ->
                    state.current
    in
        { previous = prev, current = Just backdrop }
