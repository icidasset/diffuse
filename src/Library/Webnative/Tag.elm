module Webnative.Tag exposing (..)

import Enum exposing (Enum)
import List.Zipper as Zipper exposing (Zipper)
import Playlists exposing (Playlist)



-- 🌳


type Tag
    = Mutation
      -----------------------------------------
      -- Flows
      -----------------------------------------
    | LoadPlaylists Step


type Step
    = -----------------------------------------
      -- Playlists
      -----------------------------------------
      PlaylistAdded
    | PlaylistRemoved
    | PrivatePlaylistsDirectoryCreated
    | PrivatePlaylistsDirectoryExists
    | PrivatePlaylistsDirectoryListed
    | PublicPlaylistsDirectoryCreated
    | PublicPlaylistsDirectoryExists
    | PublicPlaylistsDirectoryListed



-- 🛠


toString : Tag -> String
toString tag =
    Enum.fromIterator
        (\t ->
            case t of
                Mutation ->
                    ( "Mutation", Mutation )

                -----------------------------------------
                -- Flows
                -----------------------------------------
                LoadPlaylists step ->
                    ( "LoadPlaylists" ++ "_" ++ stepEnum.toString step, LoadPlaylists step )
        )
        Mutation


stepEnum =
    Enum.create
        [ ( "PlaylistAdded", PlaylistAdded )
        , ( "PlaylistRemoved", PlaylistRemoved )
        , ( "PrivatePlaylistsDirectoryCreated", PrivatePlaylistsDirectoryCreated )
        , ( "PrivatePlaylistsDirectoryExists", PrivatePlaylistsDirectoryExists )
        , ( "PrivatePlaylistsDirectoryListed", PrivatePlaylistsDirectoryListed )
        , ( "PublicPlaylistsDirectoryCreated", PublicPlaylistsDirectoryCreated )
        , ( "PublicPlaylistsDirectoryExists", PublicPlaylistsDirectoryExists )
        , ( "PublicPlaylistsDirectoryListed", PublicPlaylistsDirectoryListed )
        ]
