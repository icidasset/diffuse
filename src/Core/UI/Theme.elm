module UI.Theme exposing (..)

import Alfred
import Html exposing (Html)
import Material.Icons as Icons
import Themes.Sunrise.Theme as Sunrise
import Themes.Sunrise.Tracks.Scene.Covers
import Themes.Sunrise.Tracks.Scene.List
import Tracks exposing (IdentifiedTrack, Scene)
import UI.Types exposing (Model, Msg(..))


list =
    [ { id = "sunrise"
      , title = "Sunrise (default)"
      , icon = Just Icons.wb_sunny
      }
    ]


view : Model -> Html Msg
view =
    Sunrise.theme



-- TODO


scrollTracksToTop : Scene -> Cmd Msg
scrollTracksToTop scene =
    case scene of
        Tracks.Covers ->
            Themes.Sunrise.Tracks.Scene.List.scrollToTop

        Tracks.List ->
            Themes.Sunrise.Tracks.Scene.Covers.scrollToTop


scrollToNowPlaying : Scene -> IdentifiedTrack -> Model -> Cmd Msg
scrollToNowPlaying scene ( identifiers, track ) model =
    case scene of
        Tracks.Covers ->
            Themes.Sunrise.Tracks.Scene.Covers.scrollToNowPlaying
                model.viewport.width
                model.covers.harvested
                ( identifiers, track )

        Tracks.List ->
            Themes.Sunrise.Tracks.Scene.List.scrollToNowPlaying
                model.tracks.harvested
                ( identifiers, track )
