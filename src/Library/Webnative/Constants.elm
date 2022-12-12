module Webnative.Constants exposing (..)

import Webnative.AppInfo exposing (AppInfo)
import Webnative.Path as Path exposing (Directory, Path)
import Webnative.Permissions exposing (FileSystemPermissions, Permissions)


permissions : Permissions
permissions =
    { app = Just app
    , fs = Just fs
    }


app : AppInfo
app =
    { creator = "icidasset"
    , name = "Diffuse"
    }


fs : FileSystemPermissions
fs =
    { private = { directories = [ playlistsPath ], files = [] }
    , public = { directories = [ playlistsPath ], files = [] }
    }


playlistsPath : Path Directory
playlistsPath =
    Path.directory [ "Audio", "Music", "Playlists" ]
