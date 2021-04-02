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



-- ⛰


playlistPath : String -> List String
playlistPath name =
    playlistsPath ++ [ name, ".json" ]



-- ⛵️


proceed : Webnative.Response -> HypaethralBaggage -> Proceedings
proceed response baggage =
    case Debug.log "🍿" <| Webnative.decodeResponse Tag.fromString response of
        Webnative (NoArtifact LoadedFileSystemManually) ->
            --

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

        --
        Wnfs GotHypaethralData (Utf8Content json) ->
            let
                _ =
                    Debug.log "GotHypaethralData" json
            in
            -- TODO
            Hypaethral Json.null

        -----------------------------------------
        -- ...
        -----------------------------------------
        _ ->
            -- TODO: Error handling
            Hypaethral Json.null



-- 👀


retrieve : { initialised : Bool } -> HypaethralBit -> String -> Webnative.Request
retrieve { initialised } bit filename =
    if initialised then
        case bit of
            Playlists ->
                Wnfs.exists
                    Wnfs.Public
                    { path = playlistsPath
                    , tag = Tag.toString (LoadPlaylists PublicPlaylistsDirectoryExists)
                    }

            _ ->
                Wnfs.readUtf8
                    (Wnfs.AppData app)
                    { path = [ filename ]
                    , tag = Tag.toString GotHypaethralData
                    }

    else
        Webnative.loadFileSystem permissions


save : { initialised : Bool } -> HypaethralBit -> String -> Json.Value -> Webnative.Request
save { initialised } bit filename dataCollection =
    if initialised then
        case bit of
            Playlists ->
                -- Write each playlist to file
                -- TODO
                Wnfs.writeUtf8
                    (Wnfs.AppData app)
                    { path = [ filename ]
                    , tag = Tag.toString WroteHypaethralData
                    }
                    (Json.encode 0 dataCollection)

            _ ->
                Wnfs.writeUtf8
                    (Wnfs.AppData app)
                    { path = [ filename ]
                    , tag = Tag.toString WroteHypaethralData
                    }
                    (Json.encode 0 dataCollection)

    else
        Webnative.loadFileSystem permissions
