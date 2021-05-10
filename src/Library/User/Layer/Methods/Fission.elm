module User.Layer.Methods.Fission exposing (..)

import Json.Decode as Decode
import Json.Encode as Json
import List.Zipper as Zipper exposing (Zipper)
import Playlists exposing (Playlist)
import Return
import User.Layer exposing (HypaethralBaggage(..), HypaethralBit(..))
import Webnative exposing (Artifact(..), DecodedResponse(..), NoArtifact(..))
import Webnative.Constants exposing (..)
import Webnative.Path as Path exposing (Directory, File, Path)
import Webnative.Tag as Tag exposing (Step(..), Tag(..))
import Wnfs exposing (Artifact(..))



-- 🌳


type Proceedings
    = Hypaethral Json.Value
    | LoadedFileSystem
    | Ongoing HypaethralBaggage Webnative.Request
    | OtherRequest Webnative.Request
    | SaveNextHypaethralBit
    | Stopping



-- ⛰


playlistPath : String -> Path File
playlistPath name =
    playlistsPath
        |> Path.unwrap
        |> (\p -> p ++ [ name ++ ".json" ])
        |> Path.file



-- ⛵️


proceed : Webnative.Response -> HypaethralBaggage -> Proceedings
proceed response baggage =
    case Webnative.decodeResponse Tag.fromString response of
        Webnative (Webnative.NoArtifact LoadedFileSystemManually) ->
            LoadedFileSystem

        -----------------------------------------
        -- (1) Public Playlists
        -----------------------------------------
        -- Directory Exists
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

        -- List
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

        -- Read
        --
        -----------------------------------------
        -- (2) Private Playlists
        -----------------------------------------
        -- Directory Exists
        Wnfs (LoadPlaylists PrivatePlaylistsDirectoryExists) (Boolean True) ->
            { path = playlistsPath
            , tag = Tag.toString (LoadPlaylists PrivatePlaylistsDirectoryListed)
            }
                |> Wnfs.ls Wnfs.Private
                |> Ongoing baggage

        Wnfs (LoadPlaylists PrivatePlaylistsDirectoryExists) (Boolean False) ->
            finalisePlaylists baggage

        -- List
        Wnfs (LoadPlaylists PrivatePlaylistsDirectoryListed) (DirectoryContent listing) ->
            let
                _ =
                    Debug.log "PrivatePlaylistsDirectoryListed" listing
            in
            -- TODO
            Hypaethral Json.null

        -----------------------------------------
        -- Other
        -----------------------------------------
        WnfsError (Wnfs.JavascriptError "Path does not exist") ->
            Hypaethral Json.null

        Wnfs GotHypaethralData (Utf8Content json) ->
            json
                |> Decode.decodeString Decode.value
                |> Result.map Hypaethral
                |> Result.withDefault Stopping

        Wnfs PublishedHypaethralData _ ->
            SaveNextHypaethralBit

        Wnfs WroteHypaethralData Wnfs.NoArtifact ->
            { tag = Tag.toString PublishedHypaethralData }
                |> Wnfs.publish
                |> OtherRequest

        _ ->
            -- TODO: Error handling
            Stopping



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
                    { path = Path.file [ filename ]
                    , tag = Tag.toString GotHypaethralData
                    }

    else
        Webnative.loadFileSystem permissions


save : { initialised : Bool } -> HypaethralBit -> String -> Json.Value -> List Webnative.Request
save { initialised } bit filename dataCollection =
    if initialised then
        case bit of
            Playlists ->
                -- Write each playlist to a file
                dataCollection
                    |> Decode.decodeValue (Decode.list Decode.value)
                    |> Result.withDefault []
                    |> List.filterMap
                        (\playlist ->
                            playlist
                                |> Decode.decodeValue
                                    (Decode.map2
                                        Tuple.pair
                                        (Decode.field "name" Decode.string)
                                        (Decode.field "public" Decode.bool)
                                    )
                                |> Result.toMaybe
                                |> Maybe.map (Tuple.pair playlist)
                        )
                    |> List.map
                        (\( playlist, ( name, public ) ) ->
                            Wnfs.writeUtf8
                                (if public then
                                    Wnfs.Public

                                 else
                                    Wnfs.Private
                                )
                                { path = playlistPath name
                                , tag = Tag.toString WroteHypaethralData
                                }
                                (Json.encode 0 playlist)
                        )

            _ ->
                [ Wnfs.writeUtf8
                    (Wnfs.AppData app)
                    { path = Path.file [ filename ]
                    , tag = Tag.toString WroteHypaethralData
                    }
                    (Json.encode 0 dataCollection)
                ]

    else
        [ Webnative.loadFileSystem permissions ]



-- PLAYLISTS


finalisePlaylists : HypaethralBaggage -> Proceedings
finalisePlaylists baggage =
    case baggage of
        PlaylistsBaggage { publicPlaylistsRead, privatePlaylistsRead } ->
            (publicPlaylistsRead ++ privatePlaylistsRead)
                |> Json.list identity
                |> Hypaethral

        _ ->
            Hypaethral Json.null
