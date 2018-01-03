module Console.View exposing (entry)

import Console.Types exposing (Msg(..))
import Console.Styles exposing (..)
import Json.Decode as Decode
import Material.Icons.Av as Icons
import Maybe.Extra as Maybe
import Queue.Types exposing (Msg(..))
import Tracks.Types
import Tracks.Utils
import Types as TopLevel
import Utils exposing (..)
import Variables exposing (colorDerivatives, scaled)


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (on, onClick)
import Element.Ext exposing (..)
import Element.Types exposing (Attr, Node)
import Layouts
import Variables
import Variations exposing (Variations(..))


-- Styles

import Console.Styles exposing (..)
import Styles exposing (Styles(Console, Zed))


-- 🍯


entry : TopLevel.Model -> Node
entry model =
    el
        Zed
        [ center
        , maxWidth (px Variables.insulationWidth)
        , width fill
        ]
        (column
            (Console Container)
            [ center, width fill ]
            [ el Zed
                []
                (lazy2 nowPlaying model.queue.activeItem model.console.stalled)
            , el Zed
                [ width fill ]
                (lazy progress model.queue.activeItem)
            , el Zed
                [ center, maxWidth (px 432), width fill ]
                (lazy2 buttons model.queue model.console.isPlaying)
            ]
        )



-- Now playing


nowPlaying : Maybe Queue.Types.Item -> Bool -> Node
nowPlaying activeItem stalled =
    if stalled then
        el
            (Console NowPlaying)
            [ onClick (TopLevel.ConsoleMsg Unstall) ]
            (case activeItem of
                Just _ ->
                    text "Your internet connection got interrupted, click to resume."

                Nothing ->
                    text "Isotach"
            )
    else
        el
            (Console NowPlaying)
            [ paddingBottom (scaled -1), paddingTop (scaled 3) ]
            (case activeItem of
                Just item ->
                    let
                        track =
                            Tracks.Utils.unindentify item.identifiedTrack

                        t =
                            track.tags.artist ++ " – " ++ track.tags.title
                    in
                        el
                            Zed
                            [ track
                                |> Tracks.Types.ScrollToActiveTrack
                                |> TopLevel.TracksMsg
                                |> onClick
                            ]
                            (text t)

                Nothing ->
                    text "Isotach"
            )



-- Progress


progress : Maybe Queue.Types.Item -> Node
progress _ =
    el
        (Console ProgressBarContainer)
        progressAttributes
        (el
            (Console ProgressBar)
            [ height (px 3) ]
            (el
                (Console ProgressBarValue)
                [ class "progressBarValue"
                , height (px 3)
                , width (px 0)
                ]
                empty
            )
        )


progressAttributes : List Attr
progressAttributes =
    [ paddingXY 0 (scaled -10)

    --
    , TopLevel.ConsoleMsg
        |> (>>) Seek
        |> decodeClickLocation
        |> on "click"
    ]



-- Buttons


buttons : Queue.Types.Model -> Bool -> Node
buttons queue isPlaying =
    row
        Zed
        [ paddingXY 0 (scaled 1)
        , spread
        , width fill
        ]
        [ ------------------------------------
          -- Repeat
          ------------------------------------
          column
            (Console Button)
            [ buttonPadding
            , buttonSpacing
            , center
            , onClick (TopLevel.QueueMsg ToggleRepeat)
            ]
            [ el
                (Console ButtonLight)
                [ height (px lightHeight)
                , vary On queue.repeat
                , width (px lightHeight)
                ]
                empty

            --
            , 18
                |> Icons.repeat colorDerivatives.consoleText
                |> html
                |> el Zed [ moveDown 1 ]
            ]

        ------------------------------------
        -- Previous
        ------------------------------------
        , column
            (Console Button)
            [ buttonPadding
            , buttonSpacing
            , center
            , onClick (TopLevel.QueueMsg Rewind)
            ]
            [ el Zed [ height (px lightHeight) ] empty
            , 20
                |> Icons.fast_rewind colorDerivatives.consoleText
                |> html
                |> el Zed [ moveDown 1 ]
            ]

        ------------------------------------
        -- Play / Pause
        ------------------------------------
        , column
            (Console Button)
            [ buttonPadding
            , buttonSpacing
            , center

            --
            , if isPlaying then
                onClick (TopLevel.ConsoleMsg RequestPause)
              else if Maybe.isNothing queue.activeItem then
                onClick (TopLevel.QueueMsg Shift)
              else
                onClick (TopLevel.ConsoleMsg RequestPlay)
            ]
            [ el
                (Console ButtonLight)
                [ height (px lightHeight)
                , width (px 17)

                --
                , vary OnAlt isPlaying
                ]
                empty

            --
            , el
                (Console ButtonLabel)
                [ buttonPadding ]
                (text "PLAY")
            ]

        ------------------------------------
        -- Next
        ------------------------------------
        , column
            (Console Button)
            [ buttonPadding
            , buttonSpacing
            , center
            , onClick (TopLevel.QueueMsg Shift)
            ]
            [ el Zed [ height (px lightHeight) ] empty
            , 20
                |> Icons.fast_forward colorDerivatives.consoleText
                |> html
                |> el Zed [ moveDown 1 ]
            ]

        ------------------------------------
        -- Shuffle
        ------------------------------------
        , column
            (Console Button)
            [ buttonPadding
            , buttonSpacing
            , center
            , onClick (TopLevel.QueueMsg ToggleShuffle)
            ]
            [ el
                (Console ButtonLight)
                [ height (px lightHeight)
                , vary On queue.shuffle
                , width (px lightHeight)
                ]
                empty

            --
            , 18
                |> Icons.shuffle colorDerivatives.consoleText
                |> html
                |> el Zed [ moveDown 1 ]
            ]
        ]


buttonPadding : Attr
buttonPadding =
    paddingXY (scaled -5) 0


buttonSpacing : Attr
buttonSpacing =
    spacingXY 0 9



-- Events and stuff


decodeClickLocation : (Float -> msg) -> Decode.Decoder msg
decodeClickLocation message =
    Decode.map message
        (Decode.map2
            (\a b -> a / b)
            (Decode.at [ "offsetX" ] Decode.float)
            (Decode.at [ "currentTarget", "clientWidth" ] Decode.float)
        )
