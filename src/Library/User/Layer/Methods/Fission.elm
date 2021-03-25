module User.Layer.Methods.Fission exposing (..)

import Json.Encode as Json
import List.Zipper as Zipper exposing (Zipper)
import Playlists exposing (Playlist)
import Return
import User.Layer exposing (HypaethralBaggage, HypaethralBit(..))
import Webnative exposing (DecodedResponse(..))
import Webnative.Constants exposing (..)
import Webnative.Tag as Tag exposing (Step(..), Tag(..))
import Wnfs exposing (Artifact(..))



-- 🌳


type Proceedings
    = Hypaethral Json.Value
    | Ongoing HypaethralBaggage Webnative.Request



--


playlistsPath =
    [ "Audio", "Music", "Playlists" ]


playlistPath name =
    playlistsPath ++ [ name, ".json" ]



--


proceed : Webnative.Response -> HypaethralBaggage -> Proceedings
proceed response baggage =
    case Webnative.decodeResponse response of
        -----------------------------------------
        -- Private Playlists Directory Exists
        -----------------------------------------
        Wnfs (LoadPlaylists PrivatePlaylistsDirectoryExists) (Boolean True) ->
            { path = playlistsPath
            , tag = Tag.toString (LoadPlaylists PrivatePlaylistsDirectoryListed)
            }
                |> Wnfs.ls Wnfs.Private
                |> Ongoing baggage

        Wnfs (LoadPlaylists PrivatePlaylistsDirectoryExists) (Boolean False) ->
            -- TODO: Not sure this is correct
            Hypaethral Json.null

        -----------------------------------------
        -- Public Playlists Directory Exists
        -----------------------------------------
        Wnfs (LoadPlaylists PublicPlaylistsDirectoryExists) (Boolean True) ->
            { path = playlistsPath
            , tag = Tag.toString (LoadPlaylists PublicPlaylistsDirectoryListed)
            }
                |> Wnfs.ls Wnfs.Public
                |> Ongoing baggage

        Wnfs (LoadPlaylists PublicPlaylistsDirectoryExists) (Boolean False) ->
            { path = playlistsPath
            , tag = Tag.toString (LoadPlaylists PrivatePlaylistsDirectoryListed)
            }
                |> Wnfs.ls Wnfs.Private
                |> Ongoing baggage

        -----------------------------------------
        -- Directory Listings
        -----------------------------------------
        Wnfs (LoadPlaylists PublicPlaylistsDirectoryListed) (DirectoryContent listing) ->
            let
                _ =
                    Debug.log "PublicPlaylistsDirectoryListed" listing
            in
            { path = playlistsPath
            , tag = Tag.toString (LoadPlaylists PrivatePlaylistsDirectoryExists)
            }
                |> Wnfs.exists Wnfs.Private
                |> Ongoing baggage

        Wnfs (LoadPlaylists PrivatePlaylistsDirectoryListed) (DirectoryContent listing) ->
            let
                _ =
                    Debug.log "PrivatePlaylistsDirectoryListed" listing
            in
            -- TODO
            Hypaethral Json.null

        -----------------------------------------
        -- ...
        -----------------------------------------
        _ ->
            -- TODO: Error handling
            Hypaethral Json.null



--


retrieve bit filename =
    case bit of
        Playlists ->
            Wnfs.exists
                Wnfs.Public
                { path = playlistsPath
                , tag = Tag.toString (LoadPlaylists PublicPlaylistsDirectoryExists)
                }

        _ ->
            Wnfs.read
                (Wnfs.AppData app)
                { path = [ filename ]
                , tag = Tag.toString
                }


save bit dataCollection =
    Wnfs.write
