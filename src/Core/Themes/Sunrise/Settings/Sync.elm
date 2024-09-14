module Themes.Sunrise.Settings.Sync exposing (view)

import Chunky exposing (..)
import Html exposing (Html, text)
import Material.Icons.Round as Icons
import Maybe.Extra as Maybe
import Themes.Sunrise.Kit as Kit
import Themes.Sunrise.List
import Themes.Sunrise.Navigation as Navigation
import Themes.Sunrise.Settings.Common exposing (changePassphrase)
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Settings.Page exposing (Page(..))
import UI.Svg.Elements
import UI.Syncing.Common exposing (startDropbox, startIpfs, startRemoteStorage)
import UI.Syncing.Types as Syncing
import UI.Types exposing (Msg(..))
import User.Layer exposing (Method(..), dropboxMethod, ipfsMethod, methodName, remoteStorageMethod)



-- 🗺


view : Maybe Method -> Html Msg
view activeMethod =
    Kit.receptacle
        { scrolling = True }
        [ -----------------------------------------
          -- Navigation
          -----------------------------------------
          (case activeMethod of
            Just (Dropbox d) ->
                [ changePassphrase (Dropbox d) ]

            Just (Ipfs i) ->
                [ changePassphrase (Ipfs i) ]

            Just (RemoteStorage r) ->
                [ changePassphrase (RemoteStorage r) ]

            Nothing ->
                []
          )
            |> List.append
                [ ( Icon Icons.arrow_back
                  , Label "Settings" Hidden
                  , NavigateToPage (Page.Settings Index)
                  )
                , ( Icon Icons.archive
                  , Label "Data Backup" Shown
                  , NavigateToPage (Page.Settings Data)
                  )
                ]
            |> Navigation.local

        -----------------------------------------
        -- Content
        -----------------------------------------
        , chunk
            [ "relative" ]
            [ chunk
                [ "absolute", "left-0", "top-0" ]
                [ Kit.canister [ Kit.h1 "Storage Service" ] ]
            ]

        --
        , Kit.focusScreen
            { icon = Icons.account_circle
            , iconHref = Nothing
            , text =
                "By default, your playlists, favourites and other data are stored locally on your device. To ensure it is backed up, and to keep it in sync across all your connected devices, choose one of the following storage services. You can switch services at any point."
                    |> text
                    |> List.singleton
                    |> chunk [ "max-w-md" ]
                    |> List.singleton
            , textHref = Nothing
            }
            [ [ dropboxMethod
              , remoteStorageMethod
              , ipfsMethod
              ]
                |> List.map (methodView activeMethod)
                |> Themes.Sunrise.List.view Themes.Sunrise.List.Normal
                |> List.singleton
                |> chunk [ "max-w-full", "w-96" ]
            ]
        ]


methodInfoAction : Bool -> Maybe Method -> Method -> Themes.Sunrise.List.Action Msg
methodInfoAction isSelected activeMethod method =
    { icon =
        \a b ->
            inline
                [ opacity isSelected activeMethod ]
                [ Icons.help a b ]
    , msg =
        case method of
            Dropbox _ ->
                Just (\_ -> OpenUrlOnNewPage "https://dropbox.com")

            RemoteStorage _ ->
                Just (\_ -> OpenUrlOnNewPage "https://remotestorage.io")

            Ipfs _ ->
                Just (\_ -> OpenUrlOnNewPage "https://ipfs.io")
    , title =
        "Learn more about " ++ methodName method
    }


methodView activeMethod method =
    let
        isSelected =
            case ( activeMethod, method ) of
                ( Just (Dropbox _), Dropbox _ ) ->
                    True

                ( Just (RemoteStorage _), RemoteStorage _ ) ->
                    True

                ( Just (Ipfs _), Ipfs _ ) ->
                    True

                _ ->
                    False

        label =
            methodLabel activeMethod method isSelected
    in
    { label =
        case method of
            Dropbox _ ->
                label UI.Svg.Elements.dropboxLogo

            RemoteStorage _ ->
                label UI.Svg.Elements.remoteStorageLogo

            Ipfs _ ->
                label UI.Svg.Elements.ipfsLogo
    , actions =
        [ if isSelected then
            { icon = Icons.cancel
            , msg = Just (\_ -> SyncingMsg Syncing.StopSync)
            , title = "Stop syncing with " ++ methodName method
            }

          else
            methodInfoAction isSelected activeMethod method
        ]
    , msg =
        if isSelected then
            Just (SyncingMsg Syncing.StopSync)

        else if Maybe.isJust activeMethod then
            Nothing

        else
            case method of
                Dropbox _ ->
                    Just startDropbox

                RemoteStorage _ ->
                    Just startRemoteStorage

                Ipfs _ ->
                    Just startIpfs
    , isSelected =
        isSelected
    }


methodLabel activeMethod method isSelected icon =
    inline
        [ "flex", "items-center", opacity isSelected activeMethod ]
        [ inline [ "inline-block", "mr-3" ] [ Html.map never (icon 15) ]
        , Html.text (methodName method)
        ]


opacity isSelected activeMethod =
    if isSelected then
        "opacity-100"

    else if Maybe.isJust activeMethod then
        "opacity-50"

    else
        "opacity-100"
