module Abroad.State exposing (..)

import Abroad.Ports as Ports
import Abroad.Types exposing (..)
import Response.Ext exposing (do)
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { fileContents = Nothing
    , fileSelected = False
    , importMessage = Ok ""
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        FileSelectedForImport ->
            (!)
                { model | fileSelected = True }
                []

        Import ->
            (!)
                { model | importMessage = Ok "Importing ..." }
                [ Ports.importData Ports.importFileInputId ]

        ImportFinished (Just json) ->
            (!)
                { model | importMessage = Ok "Imported data successfully" }
                [ do (TopLevel.ImportUserData json) ]

        ImportFinished Nothing ->
            (!)
                { model | importMessage = Err "Import failed, could not read file" }
                []



-- 🌱


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.importDataReady ImportFinished ]
