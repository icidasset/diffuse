module Brain.Sources.Processing.Common exposing (Model, Msg(..), contextToTagsContext, isProcessing, reportError, reportHttpError, tracksFromTagsContext, translateHttpError)

import Alien
import Brain.Reply exposing (Reply(..))
import Http exposing (Error(..))
import Json.Encode as Encode
import List.Extra as List
import Maybe.Extra as Maybe
import Sources exposing (Service, Source)
import Sources.Processing exposing (..)
import Sources.Services as Services
import Time
import Tracks exposing (Track)



-- 🌳


type alias Model =
    { currentTime : Time.Posix
    , origin : String
    , status : Status
    }



-- 📣


type Msg
    = Process Arguments
    | NextInLine
      -----------------------------------------
      -- Steps
      -----------------------------------------
    | PrepareStep Context (Result Http.Error String)
    | TreeStep Context (Result Http.Error String)
    | TreeStepRemoveTracks String (List String)
    | TagsStep ContextForTags
      -----------------------------------------
      -- Bits & Pieces
      -----------------------------------------
    | SetCurrentTime Time.Posix



-- 🔱


contextToTagsContext : Context -> ContextForTags
contextToTagsContext context =
    { nextFilePaths = context.filePaths
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


reportHttpError : Source -> Http.Error -> Reply
reportHttpError source err =
    reportError
        { sourceId = source.id
        , error = translateHttpError source.service err
        }


reportError : { sourceId : String, error : String } -> Reply
reportError { sourceId, error } =
    [ ( "sourceId", Encode.string sourceId )
    , ( "error", Encode.string error )
    ]
        |> Encode.object
        |> GiveUI Alien.ReportProcessingError


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
            "Got a faulty response from this source"

        BadBody response ->
            Services.parseErrorResponse service response
