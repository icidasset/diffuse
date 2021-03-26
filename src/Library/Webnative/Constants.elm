module Webnative.Constants exposing (..)

import Webnative


permissions : Webnative.Permissions
permissions =
    { app = Just app
    , fs = Just fs
    }


app : Webnative.AppPermissions
app =
    { creator = "icidasset"
    , name = "Diffuse"
    }


fs : Webnative.FileSystemPermissions
fs =
    let
        playlists =
            String.join "/" playlistsPath
    in
    { privatePaths = [ playlists ]
    , publicPaths = [ playlists ]
    }


playlistsPath : List String
playlistsPath =
    [ "Audio", "Music", "Playlists" ]
