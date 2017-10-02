module Abroad.State exposing (..)

import Abroad.Types exposing (..)
import FileReader exposing (readAsTextFile)
import Response.Ext exposing (do)
import Task
import Types as TopLevel


-- 💧


initialModel : Model
initialModel =
    { files = []
    , importMessage = Ok ""
    }



-- 🔥


update : Msg -> Model -> ( Model, Cmd TopLevel.Msg )
update msg model =
    case msg of
        SetFiles files ->
            (!)
                { model | files = files, importMessage = Ok "" }
                []

        UploadFiles ->
            model.files
                |> List.map
                    (.blob
                        >> readAsTextFile
                        >> Task.attempt (ReadFile >> TopLevel.AbroadMsg)
                    )
                |> (!) { model | importMessage = Ok "Importing ..." }

        ReadFile (Ok str) ->
            (!)
                { model | importMessage = Ok "Imported data successfully" }
                [ do (TopLevel.ImportUserData str { store = True }) ]

        ReadFile (Err err) ->
            (!)
                { model | importMessage = Err (toString err) }
                []
