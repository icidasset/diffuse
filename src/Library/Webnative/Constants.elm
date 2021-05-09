module Webnative.Constants exposing (..)

import Webnative
import Webnative.Path as Path exposing (Directory, Path)


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
    { private = { directories = [ playlistsPath ], files = [] }
    , public = { directories = [ playlistsPath ], files = [] }
    }


playlistsPath : Path Directory
playlistsPath =
    Path.directory [ "Audio", "Music", "Playlists" ]
