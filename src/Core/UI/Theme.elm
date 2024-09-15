module UI.Theme exposing (..)

import Chunky exposing (..)
import Dict exposing (Dict)
import Html exposing (Html)
import Theme exposing (Theme)
import Themes.Sunrise.Theme as Sunrise
import Themes.Sunrise.Tracks.Scene.Covers
import Themes.Sunrise.Tracks.Scene.List
import Tracks exposing (IdentifiedTrack, Scene)
import UI.Svg.Elements
import UI.Types exposing (Model, Msg)



-- 🔮


list : List (Theme Msg Model)
list =
    [ Sunrise.theme
    ]


default : Theme Msg Model
default =
    Sunrise.theme



-- 🚧


dict : Dict String (Theme Msg Model)
dict =
    list
        |> List.map
            (\theme ->
                ( theme.id, theme )
            )
        |> Dict.fromList


view : Model -> Html Msg
view model =
    if model.isLoading then
        loadingAnimation

    else
        case model.theme of
            Just { id } ->
                case Dict.get id dict of
                    Just theme ->
                        theme.view model

                    Nothing ->
                        default.view model

            Nothing ->
                default.view model


loadingAnimation =
    chunk
        [ "flex"
        , "flex-col"
        , "items-center"
        , "justify-center"
        , "screen-height"
        , "w-screen"
        ]
        [ loadingAnimationSvg
        , chunk
            [ "italic"
            , "mt-5"
            , "text-white"
            , "text-opacity-30"
            ]
            [ Html.text "Transmitting particles" ]
        ]


loadingAnimationSvg =
    Html.map never UI.Svg.Elements.loading



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
