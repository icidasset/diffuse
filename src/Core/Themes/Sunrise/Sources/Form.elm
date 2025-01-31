module Themes.Sunrise.Sources.Form exposing (..)

import Chunky exposing (..)
import Common exposing (boolFromString, boolToString)
import Conditional exposing (..)
import Dict.Ext as Dict
import Html exposing (Html, text)
import Html.Attributes as A exposing (attribute, for, name, placeholder, required, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import List.Extra as List
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Sources exposing (..)
import Sources.Services as Services
import Themes.Sunrise.Kit as Kit exposing (ButtonType(..), select)
import Themes.Sunrise.Navigation as Navigation
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Sources.Page as Sources
import UI.Sources.Types exposing (..)



-- NEW


type alias Arguments =
    { onboarding : Bool }


new : Arguments -> Form -> List (Html Msg)
new args model =
    case model.step of
        Where ->
            newWhere args model

        How ->
            newHow model

        By ->
            newBy model


newWhere : Arguments -> Form -> List (Html Msg)
newWhere { onboarding } { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Back to list" Hidden
            --
          , if onboarding then
                NavigateToPage Page.Index

            else
                NavigateToPage (Page.Sources Sources.Index)
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form TakeStep
            [ Kit.canisterForm h ]
      )
        [ Kit.h2 "Where is your music stored?"

        -- Dropdown
        -----------
        , let
            contextServiceKey =
                Services.typeToKey context.service
          in
          Services.labels
            |> List.map
                (\( k, l ) ->
                    Html.option
                        [ value k, selected (contextServiceKey == k) ]
                        [ text l ]
                )
            |> select SelectService

        -- Button
        ---------
        , chunk
            [ "mt-10" ]
            [ Kit.button
                IconOnly
                Bypass
                (Icons.arrow_forward 17 Inherit)
            ]
        ]
    ]


newHow : Form -> List (Html Msg)
newHow { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Take a step back" Shown
          , PerformMsg TakeStepBackwards
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form TakeStep
            [ chunk
                [ "text-left", "w-full" ]
                [ Kit.canister h ]
            ]
      )
        [ Kit.h3 "Where exactly?"

        -- Note
        -------
        , note context.service

        -- Fields
        ---------
        , let
            properties =
                Services.properties context.service

            dividingPoint =
                toFloat (List.length properties) / 2

            ( listA, listB ) =
                List.splitAt (ceiling dividingPoint) properties
          in
          chunk
            [ "flex", "pt-3" ]
            [ chunk
                [ "flex-grow", "pr-4" ]
                (List.map (renderProperty context) listA)
            , chunk
                [ "flex-grow", "pl-4" ]
                (List.map (renderProperty context) listB)
            ]

        -- Button
        ---------
        , chunk
            [ "mt-3", "text-center" ]
            [ Kit.button
                IconOnly
                Bypass
                (Icons.arrow_forward 17 Inherit)
            ]
        ]
    ]


howNote : List (Html Msg) -> Html Msg
howNote =
    chunk
        [ "text-sm"
        , "italic"
        , "leading-normal"
        , "max-w-lg"
        , "mb-8"
        ]


newBy : Form -> List (Html Msg)
newBy { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Take a step back" Shown
          , PerformMsg TakeStepBackwards
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form AddSourceUsingForm
            [ Kit.canisterForm h ]
      )
        [ Kit.h2 "One last thing"
        , Kit.label [] "What are we going to call this source?"

        -- Input
        --------
        , let
            nameValue =
                Dict.fetch "name" "" context.data
          in
          chunk
            [ "flex"
            , "max-w-md"
            , "mt-8"
            , "mx-auto"
            , "justify-center"
            , "w-full"
            ]
            [ Kit.textField
                [ name "name"
                , onInput (SetFormData "name")
                , value nameValue
                ]
            ]

        -- Note
        -------
        , chunk
            [ "mt-16" ]
            (case context.service of
                AmazonS3 ->
                    corsWarning "CORS__S3"

                AzureBlob ->
                    corsWarning "CORS__Azure"

                AzureFile ->
                    corsWarning "CORS__Azure"

                Dropbox ->
                    []

                Google ->
                    []

                Ipfs ->
                    corsWarning "CORS__IPFS"

                WebDav ->
                    corsWarning "CORS__WebDAV"
            )

        -- Button
        ---------
        , Kit.button
            Normal
            Bypass
            (text "Add source")
        ]
    ]


corsWarning : String -> List (Html Msg)
corsWarning id =
    [ chunk
        [ "text-sm", "flex", "items-center", "justify-center", "leading-snug", "opacity-50" ]
        [ Kit.inlineIcon Icons.warning
        , inline
            [ "font-semibold" ]
            [ text "Make sure CORS is enabled" ]
        ]
    , chunk
        [ "text-sm", "leading-snug", "mb-8", "mt-1", "opacity-50" ]
        [ text "You can find the instructions over "
        , Kit.link { label = "here", url = "about/cors/#" ++ id }
        ]
    ]



-- EDIT


edit : Form -> List (Html Msg)
edit { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Go Back" Shown
          , PerformMsg ReturnToIndex
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form EditSourceUsingForm
            [ chunk
                [ "text-left", "w-full" ]
                [ Kit.canister h ]
            ]
      )
        [ Kit.h3 "Edit source"

        -- Note
        -------
        , note context.service

        -- Fields
        ---------
        , let
            properties =
                Services.properties context.service

            dividingPoint =
                toFloat (List.length properties) / 2

            ( listA, listB ) =
                List.splitAt (ceiling dividingPoint) properties
          in
          chunk
            [ "flex", "pt-3" ]
            [ chunk
                [ "flex-grow", "pr-4" ]
                (List.map (renderProperty context) listA)
            , chunk
                [ "flex-grow", "pl-4" ]
                (List.map (renderProperty context) listB)
            ]

        -- Button
        ---------
        , chunk
            [ "mt-3", "text-center" ]
            [ Kit.button
                Normal
                Bypass
                (text "Save")
            ]
        ]
    ]



-- PROPERTIES


renderProperty : Source -> Property -> Html Msg
renderProperty context property =
    chunk
        [ "mb-8" ]
        [ Kit.label
            [ for property.key ]
            property.label

        --
        , if
            (property.placeholder == boolToString True)
                || (property.placeholder == boolToString False)
          then
            let
                bool =
                    context.data
                        |> Dict.fetch property.key property.placeholder
                        |> boolFromString
            in
            chunk
                [ "mt-2", "pt-1" ]
                [ Kit.checkbox
                    { checked = bool
                    , toggleMsg =
                        bool
                            |> not
                            |> boolToString
                            |> SetFormData property.key
                    }
                ]

          else
            Kit.textField
                [ name property.key
                , onInput (SetFormData property.key)
                , placeholder property.placeholder
                , required (property.label |> String.toLower |> String.contains "optional" |> not)
                , type_ (ifThenElse property.password "password" "text")
                , value (Dict.fetch property.key "" context.data)

                --
                , attribute "spellcheck" "false"
                ]
        ]



-- ⚗️


form : Msg -> List (Html Msg) -> Html Msg
form msg html =
    slab
        Html.form
        [ onSubmit msg ]
        [ "flex"
        , "flex-grow"
        , "flex-shrink-0"
        , "text-center"
        ]
        [ Kit.centeredContent html ]


note : Service -> Html Msg
note service =
    case service of
        AmazonS3 ->
            nothing

        AzureBlob ->
            nothing

        AzureFile ->
            nothing

        Dropbox ->
            howNote
                [ inline
                    [ "font-semibold" ]
                    [ text "If you don't know what any of this is, "
                    , text "continue to the next screen."
                    ]
                , text " Changing the app key allows you to use your own Dropbox app."
                , text " Also, make sure you verified your email address on Dropbox,"
                , text " or this might not work."
                ]

        Google ->
            howNote
                [ inline
                    [ "font-semibold" ]
                    [ text "If you don't know what any of this is, "
                    , text "continue to the next screen."
                    ]
                , text " Changing the client stuff allows you to use your own Google OAuth client."
                , text " Disclaimer: "
                , text "The Google Drive API is fairly slow and limited, "
                , text "consider using a different service."
                ]

        Ipfs ->
            howNote
                [ inline
                    [ "font-semibold" ]
                    [ text "Diffuse will use the ipfs.io gateway by default" ]
                , text "."
                , lineBreak
                , inline
                    []
                    [ text "There are also "
                    , Html.a
                        [ A.href "https://ipfs.github.io/public-gateway-checker/"
                        , A.class "underline"
                        , A.target "_blank"
                        ]
                        [ text "other public gateways" ]
                    , text " you can choose from."
                    ]
                , lineBreak
                , text "If you would like to use another gateway, please provide it below."
                ]

        WebDav ->
            howNote
                [ inline
                    [ "font-semibold" ]
                    [ Kit.inlineIcon Icons.warning
                    , text "This app requires a proper implementation of "
                    , Kit.link
                        { label = "CORS"
                        , url = "https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS"
                        }
                    , text " on the server side."
                    ]
                , lineBreak
                , text " WebDAV servers usually don't implement"
                , text " CORS properly, if at all."
                , lineBreak
                , text " Some servers, like "
                , Kit.link
                    { label = "this one"
                    , url = "https://github.com/hacdias/webdav"
                    }
                , text ", do. You can find the configuration for that server "
                , Kit.link
                    { label = "here"
                    , url = "about/cors/#CORS__WebDAV"
                    }
                , text "."
                ]



-- RENAME


rename : Form -> List (Html Msg)
rename { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Go Back" Shown
          , PerformMsg ReturnToIndex
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form RenameSourceUsingForm
            [ Kit.canisterForm h ]
      )
        [ Kit.h2 "Name your source"

        -- Input
        --------
        , [ name "name"
          , onInput (SetFormData "name")
          , value (Dict.fetch "name" "" context.data)
          ]
            |> Kit.textField
            |> chunky [ "max-w-md", "mx-auto" ]

        -- Button
        ---------
        , chunk
            [ "mt-10" ]
            [ Kit.button
                Normal
                Bypass
                (text "Save")
            ]
        ]
    ]
