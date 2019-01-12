module UI.Tracks exposing (Model, Msg(..), initialModel, update, view)

import Chunky exposing (..)
import Html.Styled as Html exposing (Html, text)
import Replying exposing (R3D3)
import Return3
import Tracks exposing (..)
import UI.Kit
import UI.Reply exposing (Reply)



-- 🌳


type alias Model =
    { collection : Collection
    }


initialModel : Model
initialModel =
    { collection = emptyCollection
    }



-- 📣


type Msg
    = Bypass


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            Return3.withNothing model



-- 🗺


view : Model -> Html Msg
view model =
    UI.Kit.vessel []
