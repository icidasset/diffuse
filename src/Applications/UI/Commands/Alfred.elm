module UI.Commands.Alfred exposing (commands, palette)

import Alfred exposing (..)
import Conditional exposing (ifThenElse)
import Material.Icons as Icons
import Tracks
import UI.Queue.Types as Queue
import UI.Tracks.Types as Tracks
import UI.Types as UI


palette : UI.Model -> Alfred UI.Msg
palette model =
    Alfred.create
        { action = action
        , index = commands model
        , message = "Run a command."
        , operation = Query
        }



-- ⛰


commands : UI.Model -> List (Alfred.Group UI.Msg)
commands model =
    [ { name = Just "View", items = viewCommands model }
    , { name = Just "Playback", items = playbackCommands model }
    ]


playbackCommands model =
    [ if model.audioIsPlaying then
        { icon = Just (Icons.pause 16)
        , title = "Pause"
        , value = Command UI.TogglePlay
        }

      else
        { icon = Just (Icons.play_arrow 16)
        , title = "Play"
        , value = Command UI.TogglePlay
        }

    --
    , { icon = Just (Icons.fast_rewind 18)
      , title = "Previous track"
      , value = Command (UI.QueueMsg Queue.Rewind)
      }
    , { icon = Just (Icons.fast_forward 18)
      , title = "Next track"
      , value = Command (UI.QueueMsg Queue.Shift)
      }
    , { icon = Just (Icons.repeat 16)
      , title = toggle model.repeat "repeat"
      , value = Command (UI.QueueMsg Queue.ToggleRepeat)
      }
    , { icon = Just (Icons.shuffle 16)
      , title = toggle model.shuffle "shuffle"
      , value = Command (UI.QueueMsg Queue.ToggleShuffle)
      }
    ]


viewCommands model =
    [ { icon = Just (Icons.favorite 14)
      , title = toggle model.favouritesOnly "favourites-only mode"
      , value = Command (UI.TracksMsg Tracks.ToggleFavouritesOnly)
      }

    --
    , case model.scene of
        Tracks.Covers ->
            { icon = Just (Icons.notes 16)
            , title = "Switch to list view"
            , value = Command (UI.TracksMsg <| Tracks.ChangeScene Tracks.List)
            }

        Tracks.List ->
            { icon = Just (Icons.burst_mode 18)
            , title = "Switch to cover view"
            , value = Command (UI.TracksMsg <| Tracks.ChangeScene Tracks.Covers)
            }

    --
    , { icon = Just (Icons.filter_list 16)
      , title = ifThenElse model.cachedTracksOnly "Disable cached-tracks-only mode" "Only show cached tracks"
      , value = Command (UI.TracksMsg Tracks.ToggleCachedOnly)
      }
    ]



-- ㊙️


action { result } =
    case Maybe.andThen (.value >> Alfred.command) result of
        Just msg ->
            [ msg ]

        Nothing ->
            []


toggle bool suffix =
    ifThenElse bool "Disable" "Enable" ++ " " ++ suffix
