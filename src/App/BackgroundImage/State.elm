module BackgroundImage.State exposing (..)

import BackgroundImage.Types exposing (..)


-- INITIAL


initialModel : Model
initialModel =
    { imageUrl = "images/Background/1_blurred.jpg" }


initialCommands : Cmd Msg
initialCommands =
    Cmd.none



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (!) model []
