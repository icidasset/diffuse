module UI.Commands.Alfred exposing (commands, palette)

import Alfred exposing (..)
import Conditional exposing (ifThenElse)
import List.Extra as List
import Material.Icons.Round as Icons
import Playlists.Matching
import Tracks exposing (Grouping(..), SortBy(..))
import UI.Page as Page
import UI.Queue.Types as Queue
import UI.Sources.Page as Sources
import UI.Sources.Types as Sources
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
    [ { name = Just "Currently playing", items = nowPlayingCommands model }
    , { name = Just "Track selection", items = selectionCommands model }
    , { name = Just "Collection / Playlist", items = playlistCommands model }
    , { name = Just "Tracks", items = tracksCommands model }
    , { name = Just "View", items = viewCommands model }
    , { name = Just "Playback", items = playbackCommands model }
    , { name = Just "Sources", items = sourcesCommands model }
    , { name = Just "Data", items = dataCommands model }
    , { name = Just "Misc", items = miscCommands model }
    ]



--


dataCommands model =
    [ { icon = Just (Icons.offline_bolt 16)
      , title = "Clear tracks cache"
      , value = Command (UI.TracksMsg Tracks.ClearCache)
      }
    , { icon = Just (Icons.save 16)
      , title = "Download data snapshot"
      , value = Command UI.Export
      }
    , { icon = Just (Icons.save 16)
      , title = "Import data snapshot (⚠️ will override current data)"
      , value = Command UI.RequestImport
      }
    ]


miscCommands model =
    [ { icon = Just (Icons.help 16)
      , title = "Open help"
      , value = Command (UI.OpenUrlOnNewPage "./about/#UI")
      }
    ]


nowPlayingCommands : UI.Model -> List (Item UI.Msg)
nowPlayingCommands model =
    case model.nowPlaying of
        Just { item } ->
            let
                ( queueItemIdentifiers, _ ) =
                    item.identifiedTrack

                identifiedTrack =
                    model.tracks.harvested
                        |> List.getAt queueItemIdentifiers.indexInList
                        |> Maybe.withDefault item.identifiedTrack

                ( identifiers, track ) =
                    identifiedTrack
            in
            [ { icon = Just (Icons.search 16)
              , title = "Show current track in list"
              , value = Command (UI.TracksMsg Tracks.ScrollToNowPlaying)
              }

            --
            , { icon = Just (Icons.favorite 14)
              , title = ifThenElse identifiers.isFavourite "Remove favourite" "Mark as favourite"
              , value = Command (UI.TracksMsg <| Tracks.ToggleFavourite identifiers.indexInList)
              }

            --
            , { icon = Just (Icons.queue_music 16)
              , title = "Add current track to collection"
              , value = Command (UI.AssistWithAddingTracksToCollection <| [ identifiedTrack ])
              }

            --
            , { icon = Just (Icons.queue_music 16)
              , title = "Add current track to playlist"
              , value = Command (UI.AssistWithAddingTracksToPlaylist <| [ identifiedTrack ])
              }

            --
            , { icon = Just (Icons.offline_bolt 16)
              , title = "Add current track to cache"
              , value =
                    [ track ]
                        |> Tracks.StoreInCache
                        |> UI.TracksMsg
                        |> Command
              }
            ]

        Nothing ->
            []


playbackCommands model =
    [ if Maybe.map .isPlaying model.nowPlaying == Just True then
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


playlistCommands model =
    let
        selection =
            case model.selectedPlaylist of
                Just playlist ->
                    let
                        identifiedTracksFromPlaylist =
                            model.tracks.identified
                                |> Playlists.Matching.match playlist
                                |> Tuple.first

                        tracksFromPlaylist =
                            identifiedTracksFromPlaylist
                                |> (if playlist.collection then
                                        identity

                                    else
                                        Tracks.sortByIndexInPlaylist
                                   )
                                |> List.map Tuple.second
                    in
                    [ { icon = Just (Icons.waves 16)
                      , title = "Deactivate " ++ ifThenElse playlist.collection "collection" "playlist"
                      , value = Command UI.DeselectPlaylist
                      }

                    --
                    , { icon = Just (Icons.update 16)
                      , title = "Add to queue"
                      , value =
                            { inFront = False, tracks = identifiedTracksFromPlaylist }
                                |> Queue.AddTracks
                                |> UI.QueueMsg
                                |> Command
                      }

                    --
                    , { icon = Just (Icons.offline_bolt 16)
                      , title = "Store in cache"
                      , value =
                            tracksFromPlaylist
                                |> Tracks.StoreInCache
                                |> UI.TracksMsg
                                |> Command
                      }

                    --
                    , { icon = Just (Icons.archive 16)
                      , title = "Download as zip file"
                      , value =
                            tracksFromPlaylist
                                |> Tracks.Download
                                    { prefixTrackNumber = not playlist.collection
                                    , zipName = playlist.name
                                    }
                                |> UI.TracksMsg
                                |> Command
                      }

                    --
                    , { icon = Just (Icons.waves 16)
                      , title =
                            if playlist.collection then
                                "Convert to playlist"

                            else
                                "Convert to collection"
                      , value =
                            if playlist.collection then
                                { name = playlist.name }
                                    |> UI.ConvertCollectionToPlaylist
                                    |> Command

                            else
                                { name = playlist.name }
                                    |> UI.ConvertPlaylistToCollection
                                    |> Command
                      }
                    ]

                Nothing ->
                    []
    in
    [ { icon = Just (Icons.waves 16)
      , title =
            case model.selectedPlaylist of
                Just _ ->
                    "Select other collection or playlist"

                Nothing ->
                    "Select collection or playlist"
      , value = Command UI.AssistWithSelectingPlaylist
      }
    ]
        ++ selection


selectionCommands model =
    let
        ( selection, _, amountOfFavs ) =
            List.foldr
                (\( i, t ) ( acc, selected, favouriteCounter ) ->
                    case List.findIndex ((==) i.indexInList) selected of
                        Just s ->
                            ( ( i, t ) :: acc
                            , List.removeAt s selected
                            , ifThenElse i.isFavourite (favouriteCounter + 1) favouriteCounter
                            )

                        Nothing ->
                            ( acc, selected, favouriteCounter )
                )
                ( []
                , model.selectedTrackIndexes
                , 0
                )
                model.tracks.harvested
    in
    case selection of
        [] ->
            []

        tracks ->
            List.concat
                [ [ { icon = Just (Icons.queue_music 16)
                    , title = "Add current selection to collection"
                    , value = Command (UI.AssistWithAddingTracksToCollection tracks)
                    }
                  , { icon = Just (Icons.queue_music 16)
                    , title = "Add current selection to playlist"
                    , value = Command (UI.AssistWithAddingTracksToPlaylist tracks)
                    }
                  ]

                --
                , if amountOfFavs > 0 then
                    [ { icon = Just (Icons.favorite 14)
                      , title = "Remove current selection from favourites"
                      , value = Command (UI.TracksMsg <| Tracks.RemoveFavourites tracks)
                      }
                    ]

                  else
                    []

                --
                , if amountOfFavs < List.length selection then
                    [ { icon = Just (Icons.favorite 14)
                      , title = "Add current selection to favourites"
                      , value = Command (UI.TracksMsg <| Tracks.AddFavourites tracks)
                      }
                    ]

                  else
                    []
                ]


sourcesCommands model =
    [ { icon = Just (Icons.sync 16)
      , title = "Process sources"
      , value = Command (UI.SourcesMsg Sources.Process)
      }

    --
    , { icon = Just (Icons.add 16)
      , title = "Add new source"
      , value = Command (UI.ChangeUrlUsingPage <| Page.Sources Sources.New)
      }
    ]


tracksCommands model =
    let
        groupCommands =
            [ AddedOn, Directory, FirstAlphaCharacter, TrackYear ]
                |> (case model.grouping of
                        Just group ->
                            List.remove group

                        Nothing ->
                            identity
                   )
                |> List.map
                    (\group ->
                        { icon =
                            Just (Icons.library_music 16)
                        , title =
                            case group of
                                AddedOn ->
                                    "Group by processing date"

                                Directory ->
                                    "Group by directory"

                                FirstAlphaCharacter ->
                                    "Group by first letter"

                                TrackYear ->
                                    "Group by track year"
                        , value =
                            Command (UI.TracksMsg <| Tracks.GroupBy group)
                        }
                    )
                |> (\list ->
                        case model.grouping of
                            Just _ ->
                                { icon = Just (Icons.library_music 16)
                                , title = "Disable grouping"
                                , value = Command (UI.TracksMsg Tracks.DisableGrouping)
                                }
                                    :: list

                            Nothing ->
                                list
                   )
    in
    [ { icon = Just (Icons.favorite 14)
      , title = toggle model.favouritesOnly "favourites-only mode"
      , value = Command (UI.TracksMsg Tracks.ToggleFavouritesOnly)
      }

    --
    , { icon = Just (Icons.filter_list 16)
      , title = toggle model.cachedTracksOnly "cached-tracks-only mode"
      , value = Command (UI.TracksMsg Tracks.ToggleCachedOnly)
      }
    ]
        ++ groupCommands
        ++ [ { icon = Just (Icons.filter_list 16)
             , title =
                if model.hideDuplicates then
                    "Allow duplicates"

                else
                    "Remove duplicates"
             , value = Command (UI.TracksMsg Tracks.ToggleHideDuplicates)
             }

           --
           , { icon = Just (Icons.photo 16)
             , title =
                if model.coverSelectionReducesPool then
                    "Track pool is limited to selected cover (Select to disable)"

                else
                    "Track pool is not restricted by selected cover (Select to enable)"
             , value = Command (UI.TracksMsg Tracks.ToggleCoverSelectionReducesPool)
             }
           ]


viewCommands model =
    let
        sortCommands =
            (case Maybe.andThen .autoGenerated model.selectedPlaylist of
                Nothing ->
                    []

                _ ->
                    case model.scene of
                        Tracks.Covers ->
                            [ Album, Artist ]

                        Tracks.List ->
                            [ Album, Artist, Title ]
            )
                |> List.remove
                    model.sortBy
                |> List.map
                    (\sortBy ->
                        { icon =
                            Just (Icons.sort 16)
                        , title =
                            case sortBy of
                                Artist ->
                                    "Sort tracks by artist"

                                Album ->
                                    "Sort tracks by album"

                                PlaylistIndex ->
                                    "Sort tracks by playlist index"

                                Title ->
                                    "Sort tracks by title"
                        , value =
                            Command (UI.TracksMsg <| Tracks.SortBy sortBy)
                        }
                    )
    in
    [ case model.scene of
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
    , { icon = Just (Icons.sort 16)
      , title = "Change sort direction"
      , value = Command (UI.TracksMsg <| Tracks.SortBy model.sortBy)
      }
    ]
        ++ sortCommands
        ++ [ { icon = Just (Icons.brush 14)
             , title = "Change application theme"
             , value = Command UI.AssistWithChangingTheme
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
