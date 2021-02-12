module User.Layer.Methods.Fission exposing (..)

import List.Zipper as Zipper exposing (Zipper)
import Playlists exposing (Playlist)
import Webnative
import Webnative.Constants exposing (..)
import Webnative.Tag as Tag
import Wnfs






proceed flow response =
    case Fission.decodeResponse response of
        Wnfs (LoadPlaylists PublicPlaylistsDirectoryExists) (Boolean True) ->
            --


        _ ->
            Return.singleton model


--


retrieve filename =
    Wnfs.read
        (Wnfs.AppData app)
        { path = [ filename ]
        , tag = Tag.toString Tag.PublicPlaylistsDirectoryExists
        }


save bit dataCollection =
    Wnfs.write
