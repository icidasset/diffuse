module Brain.Tracks exposing (IndexedTrack, Model, Msg(..), createSearchIndex, initialModel, update)

import Alien
import Brain.Reply exposing (Reply(..))
import ElmTextSearch
import Json.Encode as Json
import Replying exposing (R3D3)
import Tracks exposing (Track)



-- 🌳


type alias Model =
    { searchIndex : ElmTextSearch.Index IndexedTrack
    }


type alias IndexedTrack =
    { ref : String

    --
    , album : String
    , artist : String
    , title : String
    }


initialModel : Model
initialModel =
    { searchIndex = createSearchIndex []
    }



-- 📣


type Msg
    = Search String
    | UpdateSearchIndex (List Track)


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        Search term ->
            let
                ( updatedIndex, results ) =
                    Result.withDefault
                        ( model.searchIndex, [] )
                        (ElmTextSearch.search
                            term
                            model.searchIndex
                        )

                json =
                    results
                        |> List.map Tuple.first
                        |> Json.list Json.string
            in
            ( { model | searchIndex = updatedIndex }
            , Cmd.none
            , Just [ GiveUI Alien.SearchTracks json ]
            )

        UpdateSearchIndex tracks ->
            ( { model | searchIndex = createSearchIndex tracks }
            , Cmd.none
            , Nothing
            )



-- SEARCH


createSearchIndex : List Track -> ElmTextSearch.Index IndexedTrack
createSearchIndex tracks =
    { ref = .ref
    , fields =
        [ ( .album, 5.0 )
        , ( .artist, 5.0 )
        , ( .title, 5.0 )
        ]
    , listFields = []
    }
        |> ElmTextSearch.new
        |> ElmTextSearch.addDocs (List.map makeIndexedTrack tracks)
        |> Tuple.first


makeIndexedTrack : Track -> IndexedTrack
makeIndexedTrack track =
    { ref = track.id

    --
    , artist = track.tags.artist
    , album = track.tags.album
    , title = track.tags.title
    }
