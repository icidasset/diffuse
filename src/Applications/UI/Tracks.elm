module UI.Tracks exposing (Model, Msg(..), initialModel, makeParcel, resolveParcel, update, view)

import Chunky exposing (..)
import Html.Styled as Html exposing (Html, text)
import Json.Decode
import Replying exposing (R3D3)
import Return3
import Tracks exposing (..)
import Tracks.Collection exposing (..)
import Tracks.Encoding as Encoding
import UI.Kit
import UI.Reply exposing (Reply(..))



-- 🌳


type alias Model =
    { collection : Collection
    , enabledSourceIds : List String
    , favourites : List Favourite
    , favouritesOnly : Bool
    , nowPlaying : Maybe IdentifiedTrack
    , searchResults : Maybe (List String)
    , searchTerm : Maybe String
    , sortBy : SortBy
    , sortDirection : SortDirection
    }


initialModel : Model
initialModel =
    { collection = emptyCollection
    , enabledSourceIds = []
    , favourites = []
    , favouritesOnly = False
    , nowPlaying = Nothing
    , searchResults = Nothing
    , searchTerm = Nothing
    , sortBy = Artist
    , sortDirection = Asc
    }



-- 📣


type Msg
    = Bypass
      -----------------------------------------
      -- Collection, Pt. 1
      -----------------------------------------
      -----------------------------------------
      -- Collection, Pt. 2
      -----------------------------------------
    | Add Json.Decode.Value


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            Return3.withNothing model

        -----------------------------------------
        -- Collection, Pt. 1
        -----------------------------------------
        -----------------------------------------
        -- Collection, Pt. 2
        -----------------------------------------
        -- # Add
        -- > Add tracks to the collection.
        --
        Add json ->
            let
                tracks =
                    json
                        |> Json.Decode.decodeValue (Json.Decode.list Encoding.trackDecoder)
                        |> Result.withDefault []
            in
            model
                |> makeParcel
                |> add tracks
                |> resolveParcel model



-- 📣  ░░  PARCEL


makeParcel : Model -> Parcel
makeParcel model =
    ( { enabledSourceIds = model.enabledSourceIds
      , favourites = model.favourites
      , favouritesOnly = model.favouritesOnly
      , nowPlaying = model.nowPlaying
      , searchResults = model.searchResults
      , sortBy = model.sortBy
      , sortDirection = model.sortDirection
      }
    , model.collection
    )


resolveParcel : Model -> Parcel -> R3D3 Model Msg Reply
resolveParcel model ( _, newCollection ) =
    let
        modelWithNewCollection =
            { model | collection = newCollection }
    in
    if model.collection.untouched /= newCollection.untouched then
        ( modelWithNewCollection
        , Cmd.none
        , Just [ SaveHypaethralUserData ]
        )

    else
        Return3.withNothing modelWithNewCollection



-- 🗺


view : Model -> Html Msg
view model =
    raw
        (List.map
            (\t -> text t.tags.title)
            model.collection.untouched
        )
