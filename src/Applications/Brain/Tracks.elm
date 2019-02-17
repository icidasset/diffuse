module Brain.Tracks exposing (Model, Msg(..), initialModel, subscriptions, update)

import Alien
import Brain.Ports as Ports
import Brain.Reply exposing (Reply(..))
import Json.Encode as Json
import Replying exposing (R3D3)
import Tracks exposing (Track)



-- 🌳


type alias Model =
    {}


initialModel : Model
initialModel =
    {}



-- 📣


type Msg
    = Search String
    | Searched (List String)
    | UpdateSearchIndex Json.Value


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Search term ->
            ( model
            , Ports.requestSearch term
            , Nothing
            )

        Searched results ->
            ( model
            , Cmd.none
            , Just [ GiveUI Alien.SearchTracks <| Json.list Json.string results ]
            )

        UpdateSearchIndex tracksJson ->
            ( model
            , Ports.updateSearchIndex tracksJson
            , Nothing
            )



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveSearchResults Searched
