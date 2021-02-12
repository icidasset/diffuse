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
    { privatePaths = [ playlistsPath ]
    , publicPaths = [ playlistsPath ]
    }


playlistsPath : String
playlistsPath =
    "/Audio/Music/Playlists"
