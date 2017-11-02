module Slave.State exposing (..)

import Json.Decode as Decode exposing (..)
import Slave.Translations as Translations
import Types as TopLevel exposing (AlienEvent)


-- 💧


initialModel : Model
initialModel =
    { sourceProcessing = Sources.Processing.State.initialModel }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none



-- 🔥


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Extraterrestrial ProcessSources (Ok result) ->
            case decodeValue (dict string) result of
                -- 🚀
                --
                Ok dict ->
                    (!) model []

                -- ⚠️
                --
                Err _ ->
                    (!) model []

        Extraterrestrial ProcessSources (Err _) ->
            (!) model []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ incoming handleAlienEvent ]


handleAlienEvent : AlienEvent -> Msg
handleAlienEvent event =
    Extraterrestrial
        (Translations.stringToAlienMessage event.tag)
        (case event.error of
            Just err ->
                Err err

            Nothing ->
                Ok event.data
        )
