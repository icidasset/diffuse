module Webnative.Tag exposing (..)

import Enum exposing (Enum)
import List.Zipper as Zipper exposing (Zipper)
import Playlists exposing (Playlist)



-- 🌳


type Tag
    = GotHypaethralData
    | Published
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
    [ GotHypaethralData
    , Published
    , WroteHypaethralData

    -----------------------------------------
    -- Playlists
    -----------------------------------------
    , LoadPlaylists PlaylistAdded
    , LoadPlaylists PlaylistRemoved
    , LoadPlaylists PrivatePlaylistsDirectoryCreated
    , LoadPlaylists PrivatePlaylistsDirectoryExists
    , LoadPlaylists PrivatePlaylistsDirectoryListed
    , LoadPlaylists PublicPlaylistsDirectoryCreated
    , LoadPlaylists PublicPlaylistsDirectoryExists
    , LoadPlaylists PublicPlaylistsDirectoryListed
    ]
        |> List.map tagIterator
        |> Enum.create


tagIterator tag =
    case tag of
        GotHypaethralData ->
            ( "GotHypaethralData", GotHypaethralData )

        Published ->
            ( "Published", Published )

        WroteHypaethralData ->
            ( "WroteHypaethralData", WroteHypaethralData )

        -----------------------------------------
        -- Flows
        -----------------------------------------
        LoadPlaylists step ->
            ( "LoadPlaylists" ++ "_" ++ stepEnum.toString step
            , LoadPlaylists step
            )


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
