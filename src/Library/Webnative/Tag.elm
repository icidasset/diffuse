module Webnative.Tag exposing (..)

import Enum exposing (Enum)
import List.Zipper as Zipper exposing (Zipper)
import Playlists exposing (Playlist)



-- 🌳


type Tag
    = GotHypaethralData
    | WroteHypaethralData
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


fromString : String -> Result String Tag
fromString =
    tagEnum.fromString >> Result.fromMaybe "Unknown tag"


toString : Tag -> String
toString =
    tagEnum.toString



-- ENUMS


tagEnum =
    Enum.fromIterator
        (\t ->
            case t of
                GotHypaethralData ->
                    ( "GotHypaethralData", GotHypaethralData )

                WroteHypaethralData ->
                    ( "WroteHypaethralData", WroteHypaethralData )

                -----------------------------------------
                -- Flows
                -----------------------------------------
                LoadPlaylists step ->
                    ( "LoadPlaylists" ++ "_" ++ stepEnum.toString step, LoadPlaylists step )
        )
        GotHypaethralData


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
