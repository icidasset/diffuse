module Brain.Sources.Processing.Common exposing (..)

import Alien
import Brain.Common.State as Common
import Brain.Types exposing (Manager)
import Dict.Ext as Dict
import Http exposing (Error(..))
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Sources exposing (Service, Source)
import Sources.Processing exposing (..)
import Sources.Services as Services
import Tracks exposing (Track)



-- 🔱


contextToTagsContext : Context -> ContextForTags
contextToTagsContext context =
    { amount = List.length context.filePaths
    , nextFilePaths = context.filePaths
    , receivedFilePaths = []
    , receivedTags = []
    , sourceId = context.source.id
    , urlsForTags = []
    }


isProcessing : Status -> Bool
isProcessing status =
    case status of
        Processing _ _ ->
            True

        NotProcessing ->
            False


reportHttpError : Source -> Http.Error -> Manager
reportHttpError source err =
    reportError
        source
        (translateHttpError source.service err)


reportError : Source -> String -> Manager
reportError source error =
    [ ( "sourceId", Encode.string source.id )
    , ( "sourceName", Encode.string (Dict.fetch "name" "Unnamed" source.data) )
    , ( "error", Encode.string error )
    ]
        |> Encode.object
        |> Common.giveUI Alien.ReportProcessingError


tracksFromTagsContext : ContextForTags -> List Track
tracksFromTagsContext context =
    context.receivedTags
        |> List.zip context.receivedFilePaths
        |> List.filter (Tuple.second >> Maybe.isJust)
        |> List.map (Tuple.mapSecond (Maybe.withDefault Tracks.emptyTags))
        |> List.map (Tracks.makeTrack context.sourceId)


translateHttpError : Service -> Http.Error -> String
translateHttpError service err =
    case err of
        NetworkError ->
            "Cannot connect to this source"

        Timeout ->
            "Source did not respond (timeout)"

        BadUrl _ ->
            "Diffuse error, invalid url was used"

        BadStatus _ ->
            "Got a faulty response from this source. Use the developer console to get more info."

        BadBody response ->
            response
                |> Services.parseErrorResponse service
                |> Maybe.withDefault (translateHttpError service <| BadStatus 0)
