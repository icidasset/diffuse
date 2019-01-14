module UI.UserData exposing (exportHypaethral, importHypaethral)

{-| Import user data into or export user data from the UI.Core.Model
-}

import Authentication exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Replying exposing (R3D3)
import Sources.Encoding as Sources
import Tracks exposing (emptyCollection)
import Tracks.Collection as Tracks
import Tracks.Encoding as Tracks
import UI.Core
import UI.Reply as UI
import UI.Sources as Sources
import UI.Tracks as Tracks



-----------------------------------------
-- 📭
-----------------------------------------


importHypaethral : Decode.Value -> UI.Core.Model -> R3D3 UI.Core.Model UI.Core.Msg UI.Reply
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



-- ░░  IMPORTING HYPAETHRAL


importSources : Sources.Model -> HypaethralUserData -> R3D3 Sources.Model Sources.Msg UI.Reply
importSources model data =
    ( { model
        | collection = Maybe.withDefault [] data.sources
      }
    , Cmd.none
    , Nothing
    )


importTracks : Tracks.Model -> HypaethralUserData -> R3D3 Tracks.Model Tracks.Msg UI.Reply
importTracks model data =
    let
        tracks =
            Maybe.withDefault [] data.tracks

        adjustedModel =
            { model
                | collection = { emptyCollection | untouched = tracks }
                , favourites = Maybe.withDefault [] data.favourites
            }
    in
    adjustedModel
        |> Tracks.makeParcel
        |> Tracks.identify
        |> Tracks.resolveParcel adjustedModel



-- ░░  DECODING


decode : Decode.Value -> Result Decode.Error HypaethralUserData
decode =
    Decode.decodeValue decoder


decoder : Decode.Decoder HypaethralUserData
decoder =
    Decode.map3
        HypaethralUserData
        (Decode.maybe <| Decode.field "favourites" <| Decode.list Tracks.favouriteDecoder)
        (Decode.maybe <| Decode.field "sources" <| Decode.list Sources.decoder)
        (Decode.maybe <| Decode.field "tracks" <| Decode.list Tracks.trackDecoder)



-- ░░  FALLBACKS


emptyHypaethralUserData : HypaethralUserData
emptyHypaethralUserData =
    { favourites = Nothing
    , sources = Nothing
    , tracks = Nothing
    }



-----------------------------------------
-- 📮
-----------------------------------------


exportHypaethral : UI.Core.Model -> Encode.Value
exportHypaethral =
    encode



-- ░░  ENCODING


encode : UI.Core.Model -> Encode.Value
encode model =
    Encode.object
        [ ( "favourites", Encode.list Tracks.encodeFavourite model.tracks.favourites )
        , ( "sources", Encode.list Sources.encode model.sources.collection )
        , ( "tracks", Encode.list Tracks.encodeTrack model.tracks.collection.untouched )
        ]
