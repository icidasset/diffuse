module Brain.Sources.Processing exposing (Model, Msg(..), initialCommand, initialModel, subscriptions, update)

import Brain.Reply exposing (Reply(..))
import Http exposing (Error(..))
import Json.Encode as Encode
import Replying exposing (R3D3)
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


initialModel : Model
initialModel =
    { currentTime = Time.millisToPosix 0
    , origin = "ORIGIN_UNKNOWN"
    , status = NotProcessing
    }


initialCommand : Cmd Msg
initialCommand =
    Cmd.none



-- 📣


type Msg
    = Process Arguments
    | NextInLine
      -----------------------------------------
      -- Steps
      -----------------------------------------
    | PrepareStep Context (Result Http.Error String)
    | TreeStep Context (Result Http.Error String)
    | TagsStep ContextForTags
      -----------------------------------------
      -- Bits & Pieces
      -----------------------------------------
    | SetCurrentTime Time.Posix


update : Msg -> Model -> R3D3 Model Msg Reply
update msg model =
    case msg of
        {- If already processing, do nothing.
           If there are no sources, do nothing.
           If there are sources, start processing the first source.
        -}
        Process { origin, sources, tracks } ->
            ( model
            , Cmd.none
            , Nothing
            )

        {- If not processing, do nothing.
           If there are no sources left, do nothing.
           If there are sources left, start processing the next source in line.
        -}
        NextInLine ->
            ( model
            , Cmd.none
            , Nothing
            )

        -----------------------------------------
        -- Phase 1
        -- Prepare for processing.
        -----------------------------------------
        PrepareStep context (Ok response) ->
            ( model
            , Cmd.none
            , Nothing
            )

        PrepareStep context (Err err) ->
            ( model
            , Cmd.none
            , Just [ reportHttpError context.source err ]
            )

        -----------------------------------------
        -- Phase 2
        -- Make a file list/tree.
        -----------------------------------------
        TreeStep context (Ok response) ->
            ( model
            , Cmd.none
            , Nothing
            )

        TreeStep context (Err err) ->
            ( model
            , Cmd.none
            , Just [ reportHttpError context.source err ]
            )

        -----------------------------------------
        -- Phase 3
        -- Get the tags for each file in the file list.
        -----------------------------------------
        TagsStep tagsContext ->
            ( model
            , Cmd.none
            , Nothing
            )

        -----------------------------------------
        -- Bits & Pieces
        -----------------------------------------
        SetCurrentTime time ->
            ( { model | currentTime = time }
            , Cmd.none
            , Nothing
            )



-- 📣 ▒▒▒ COMMON


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
        |> ReportSourceProcessingError


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



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (60 * 1000) SetCurrentTime
