module UI.UserData exposing (encodedFavourites, encodedSources, encodedTracks, importHypaethral)

import Authentication exposing (..)
import Json.Decode as Json
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Replying exposing (R3D3)
import Sources.Encoding as Sources
import Tracks exposing (emptyCollection)
import Tracks.Collection as Tracks
import Tracks.Encoding as Tracks
import UI.Core
import UI.Reply as UI
import UI.Sources as Sources
import UI.Tracks as Tracks



-- 🔱


encodedFavourites : UI.Core.Model -> Json.Value
encodedFavourites { tracks } =
    Json.Encode.list Tracks.encodeFavourite tracks.favourites


encodedSources : UI.Core.Model -> Json.Value
encodedSources { sources } =
    Json.Encode.list Sources.encode sources.collection


encodedTracks : UI.Core.Model -> Json.Value
encodedTracks { tracks } =
    Json.Encode.list Tracks.encodeTrack tracks.collection.untouched


importHypaethral : Json.Value -> UI.Core.Model -> R3D3 UI.Core.Model UI.Core.Msg UI.Reply
importHypaethral value model =
    let
        data =
            Result.withDefault emptyHypaethralUserData (decode value)

        ( sourcesModel, sourcesCmd, sourcesReply ) =
            importSources model.sources data

        ( tracksModel, tracksCmd, tracksReply ) =
            importTracks model.tracks data
    in
    ( { model
        | sources = sourcesModel
        , tracks = tracksModel
      }
    , Cmd.batch
        [ Cmd.map UI.Core.SourcesMsg sourcesCmd
        , Cmd.map UI.Core.TracksMsg tracksCmd
        ]
    , mergeReplies
        [ sourcesReply
        , tracksReply
        ]
    )



-- ㊙️


importSources : Sources.Model -> HypaethralUserData -> R3D3 Sources.Model Sources.Msg UI.Reply
importSources model data =
    ( { model
        | collection = data.sources
      }
    , Cmd.none
    , Nothing
    )


importTracks : Tracks.Model -> HypaethralUserData -> R3D3 Tracks.Model Tracks.Msg UI.Reply
importTracks model data =
    let
        adjustedModel =
            { model
                | collection = { emptyCollection | untouched = data.tracks }
                , favourites = data.favourites
            }
    in
    adjustedModel
        |> Tracks.makeParcel
        |> Tracks.identify
        |> Tracks.resolveParcel adjustedModel


mergeReplies : List (Maybe (List UI.Reply)) -> Maybe (List UI.Reply)
mergeReplies list =
    list
        |> List.foldl
            (\maybeReply replies ->
                case maybeReply of
                    Just r ->
                        replies ++ r

                    Nothing ->
                        replies
            )
            []
        |> Just
