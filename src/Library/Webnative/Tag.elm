module Webnative.Tag exposing (..)

import Enum



-- 🌳


type Tag
    = GotHypaethralData
    | PublishedHypaethralData
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
    | PrivatePlaylistRead
    | PublicPlaylistsDirectoryCreated
    | PublicPlaylistsDirectoryExists
    | PublicPlaylistsDirectoryListed
    | PublicPlaylistRead



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
    , PublishedHypaethralData
    , WroteHypaethralData

    -----------------------------------------
    -- Playlists
    -----------------------------------------
    , LoadPlaylists PlaylistAdded
    , LoadPlaylists PlaylistRemoved
    , LoadPlaylists PrivatePlaylistsDirectoryCreated
    , LoadPlaylists PrivatePlaylistsDirectoryExists
    , LoadPlaylists PrivatePlaylistsDirectoryListed
    , LoadPlaylists PrivatePlaylistRead
    , LoadPlaylists PublicPlaylistsDirectoryCreated
    , LoadPlaylists PublicPlaylistsDirectoryExists
    , LoadPlaylists PublicPlaylistsDirectoryListed
    , LoadPlaylists PublicPlaylistRead
    ]
        |> List.map tagIterator
        |> Enum.create


tagIterator tag =
    case tag of
        GotHypaethralData ->
            ( "GotHypaethralData", GotHypaethralData )

        PublishedHypaethralData ->
            ( "PublishedHypaethralData", PublishedHypaethralData )

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
        , ( "PrivatePlaylistRead", PrivatePlaylistRead )
        , ( "PublicPlaylistsDirectoryCreated", PublicPlaylistsDirectoryCreated )
        , ( "PublicPlaylistsDirectoryExists", PublicPlaylistsDirectoryExists )
        , ( "PublicPlaylistsDirectoryListed", PublicPlaylistsDirectoryListed )
        , ( "PublicPlaylistRead", PublicPlaylistRead )
        ]
