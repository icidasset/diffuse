module Brain.Tracks exposing (Model, Msg(..), initialModel, subscriptions, update)

import Alien
import Brain.Ports as Ports
import Brain.Reply exposing (Reply(..))
import Json.Encode as Json
import Return3 as Return exposing (..)



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


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        Search term ->
            ( model
            , Ports.requestSearch term
            , []
            )

        Searched results ->
            ( model
            , Cmd.none
            , [ GiveUI Alien.SearchTracks (Json.list Json.string results) ]
            )

        UpdateSearchIndex tracksJson ->
            ( model
            , Ports.updateSearchIndex tracksJson
            , []
            )



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.receiveSearchResults Searched
