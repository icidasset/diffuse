module ContextMenu.State exposing (..)

import ContextMenu.Types exposing (..)
import Playlists.ContextMenu
import Sources.ContextMenu
import Types as TopLevel


-- 💧


initialModel : Model TopLevel.Msg
initialModel =
    { instance = Nothing
    }



-- 🔥


update : Msg -> Model TopLevel.Msg -> ( Model TopLevel.Msg, Cmd TopLevel.Msg )
update msg model =
    case msg of
        Hide ->
            (!) { model | instance = Nothing } []

        ShowPlaylistMenu playlistName mousePos ->
            let
                instance =
                    mousePos
                        |> Playlists.ContextMenu.listMenu playlistName
                        |> Just
            in
                (!) { model | instance = instance } []

        ShowSourceMenu sourceId mousePos ->
            let
                instance =
                    mousePos
                        |> Sources.ContextMenu.listMenu sourceId
                        |> Just
            in
                (!) { model | instance = instance } []
