module UI.Sources.Form exposing (FormStep(..), Model, Msg(..), defaultContext, edit, initialModel, new, takeStepBackwards, takeStepForwards, update)

import Chunky exposing (..)
import Common exposing (boolFromString, boolToString)
import Conditional exposing (..)
import Css.Classes as C
import Dict
import Dict.Ext as Dict
import Html exposing (Html, strong, text)
import Html.Attributes exposing (for, name, placeholder, required, selected, type_, value)
import Html.Events exposing (onInput, onSubmit)
import List.Extra as List
import Material.Icons exposing (Coloring(..))
import Material.Icons.Alert as Icons
import Material.Icons.Navigation as Icons
import Return3 exposing (..)
import Sources exposing (..)
import Sources.Services as Services
import Sources.Services.Dropbox
import Sources.Services.Google
import Tachyons.Classes as T
import UI.Kit exposing (ButtonType(..), select)
import UI.Navigation exposing (..)
import UI.Page as Page
import UI.Reply exposing (Reply(..))
import UI.Sources.Page as Sources



-- 🌳


type alias Model =
    { step : FormStep
    , context : Source
    }


type FormStep
    = Where
    | How
    | By


initialModel : Model
initialModel =
    { step = Where
    , context = defaultContext
    }


defaultContext : Source
defaultContext =
    { id = "CHANGE_ME_PLEASE"
    , data = Services.initialData defaultService
    , directoryPlaylists = True
    , enabled = True
    , service = defaultService
    }


defaultService : Service
defaultService =
    Dropbox



-- 📣


type Msg
    = AddSource
    | Bypass
    | EditSource
    | ReturnToIndex
    | SelectService String
    | SetData String String
    | TakeStep
    | TakeStepBackwards


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        AddSource ->
            let
                context =
                    model.context

                cleanContext =
                    { context | data = Dict.map (always String.trim) context.data }
            in
            returnRepliesWithModel
                { model | step = Where, context = defaultContext }
                [ GoToPage (Page.Sources Sources.Index)
                , AddSourceToCollection cleanContext
                ]

        Bypass ->
            return model

        EditSource ->
            returnRepliesWithModel
                { model | step = Where, context = defaultContext }
                [ ReplaceSourceInCollection model.context
                , ProcessSources [ model.context ]
                , GoToPage (Page.Sources Sources.Index)
                ]

        ReturnToIndex ->
            returnRepliesWithModel
                model
                [ GoToPage (Page.Sources Sources.Index) ]

        SelectService serviceKey ->
            case Services.keyToType serviceKey of
                Just service ->
                    let
                        ( context, data ) =
                            ( model.context
                            , Services.initialData service
                            )

                        newContext =
                            { context | data = data, service = service }
                    in
                    return { model | context = newContext }

                Nothing ->
                    return model

        SetData key value ->
            let
                context =
                    model.context

                updatedData =
                    Dict.insert key value context.data

                newContext =
                    { context | data = updatedData }
            in
            return { model | context = newContext }

        TakeStep ->
            case ( model.step, model.context.service ) of
                ( How, Dropbox ) ->
                    model.context.data
                        |> Sources.Services.Dropbox.authorizationUrl
                        |> ExternalSourceAuthorization
                        |> returnReplyWithModel model

                ( How, Google ) ->
                    model.context.data
                        |> Sources.Services.Google.authorizationUrl
                        |> ExternalSourceAuthorization
                        |> returnReplyWithModel model

                _ ->
                    return { model | step = takeStepForwards model.step }

        TakeStepBackwards ->
            return { model | step = takeStepBackwards model.step }


takeStepForwards : FormStep -> FormStep
takeStepForwards currentStep =
    case currentStep of
        Where ->
            How

        _ ->
            By


takeStepBackwards : FormStep -> FormStep
takeStepBackwards currentStep =
    case currentStep of
        By ->
            How

        _ ->
            Where



-- NEW


type alias Arguments =
    { onboarding : Bool }


new : Arguments -> Model -> List (Html Msg)
new args model =
    case model.step of
        Where ->
            newWhere args model

        How ->
            newHow model

        By ->
            newBy model


newWhere : Arguments -> Model -> List (Html Msg)
newWhere { onboarding } { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
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
            [ UI.Kit.canisterForm h ]
      )
        [ UI.Kit.h2 "Where is your music stored?"

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
            [ T.mt4, C.pt_2 ]
            [ UI.Kit.button
                IconOnly
                Bypass
                (Icons.arrow_forward 17 Inherit)
            ]
        ]
    ]


newHow : Model -> List (Html Msg)
newHow { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
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
                [ T.tl, T.w_100 ]
                [ UI.Kit.canister h ]
            ]
      )
        [ UI.Kit.h3 "Where exactly?"

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
            [ C.flex, C.pt_3 ]
            [ chunk
                [ C.flex_grow, T.pr3 ]
                (List.map (renderProperty context) listA)
            , chunk
                [ C.flex_grow, T.pl3 ]
                (List.map (renderProperty context) listB)
            ]

        -- Button
        ---------
        , chunk
            [ C.mt_3, C.text_center ]
            [ UI.Kit.button
                IconOnly
                Bypass
                (Icons.arrow_forward 17 Inherit)
            ]
        ]
    ]


howNote : List (Html Msg) -> Html Msg
howNote =
    chunk [ C.text_sm, T.i, C.leading_normal, T.mb4, T.measure_wide ]


newBy : Model -> List (Html Msg)
newBy { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Take a step back" Shown
          , PerformMsg TakeStepBackwards
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form AddSource
            [ UI.Kit.canisterForm h ]
      )
        [ UI.Kit.h2 "One last thing"
        , UI.Kit.label [] "What are we going to call this source?"

        -- Input
        --------
        , let
            nameValue =
                Dict.fetch "name" "" context.data
          in
          chunk
            [ C.flex, T.mt4, C.justify_center, T.w_100 ]
            [ UI.Kit.textField
                [ name "name"
                , onInput (SetData "name")
                , value nameValue
                ]
            ]

        -- Note
        -------
        , chunk
            [ T.mt5 ]
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
        , UI.Kit.button
            Normal
            Bypass
            (text "Add source")
        ]
    ]


corsWarning : String -> List (Html Msg)
corsWarning id =
    [ chunk
        [ C.text_sm, C.flex, C.items_center, C.justify_center, C.leading_snug, T.o_50 ]
        [ UI.Kit.inlineIcon Icons.warning
        , strong
            []
            [ text "Make sure CORS is enabled" ]
        ]
    , chunk
        [ C.text_sm, C.leading_snug, T.mb4, T.mt1, T.o_50 ]
        [ text "You can find the instructions over "
        , UI.Kit.link { label = "here", url = "about#" ++ id }
        ]
    ]



-- EDIT


edit : Model -> List (Html Msg)
edit { context } =
    [ -----------------------------------------
      -- Navigation
      -----------------------------------------
      UI.Navigation.local
        [ ( Icon Icons.arrow_back
          , Label "Go Back" Shown
          , PerformMsg ReturnToIndex
          )
        ]

    -----------------------------------------
    -- Content
    -----------------------------------------
    , (\h ->
        form EditSource
            [ chunk
                [ T.tl, T.w_100 ]
                [ UI.Kit.canister h ]
            ]
      )
        [ UI.Kit.h3 "Edit source"

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
            [ C.flex, C.pt_3 ]
            [ chunk
                [ C.flex_grow, T.pr3 ]
                (List.map (renderProperty context) listA)
            , chunk
                [ C.flex_grow, T.pl3 ]
                (List.map (renderProperty context) listB)
            ]

        -- Button
        ---------
        , chunk
            [ C.mt_3, C.text_center ]
            [ UI.Kit.button
                Normal
                Bypass
                (text "Save")
            ]
        ]
    ]



-- PROPERTIES


renderProperty : Source -> Property -> Html Msg
renderProperty context property =
    -- TODO
    -- chunk
    --     [ T.mb4 ]
    --     [ UI.Kit.label
    --         [ for property.key ]
    --         property.label
    --
    --     --
    --     , if
    --         (property.placeholder == boolToString True)
    --             || (property.placeholder == boolToString False)
    --       then
    --         let
    --             bool =
    --                 context.data
    --                     |> Dict.fetch property.key property.placeholder
    --                     |> boolFromString
    --         in
    --         chunk
    --             [ T.mt2, C.pt_1 ]
    --             [ UI.Kit.checkbox
    --                 { checked = bool
    --                 , toggleMsg =
    --                     bool
    --                         |> not
    --                         |> boolToString
    --                         |> SetData property.key
    --                 }
    --             ]
    --
    --       else
    --         UI.Kit.textField
    --             [ name property.key
    --             , onInput (SetData property.key)
    --             , placeholder property.placeholder
    --             , required (property.label |> String.toLower |> String.contains "optional" |> not)
    --             , type_ (ifThenElse property.password "password" "text")
    --             , value (Dict.fetch property.key "" context.data)
    --             ]
    --     ]
    nothing



-- ⚗️


form : Msg -> List (Html Msg) -> Html Msg
form msg html =
    slab
        Html.form
        [ onSubmit msg ]
        [ C.flex
        , C.flex_grow
        , C.flex_shrink_0
        , C.text_center
        ]
        [ UI.Kit.centeredContent html ]


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
                    [ C.font_semibold ]
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
                    [ C.font_semibold ]
                    [ text "If you don't know what any of this is, "
                    , text "continue to the next screen."
                    ]
                , text " Changing the client stuff allows you to use your own Google OAuth client."
                ]

        Ipfs ->
            howNote
                [ inline [ C.font_semibold ] [ text "Diffuse will try to use the default local gateway" ]
                , text "."
                , lineBreak
                , text "If you would like to use another gateway, please provide it below."
                ]

        WebDav ->
            howNote
                [ inline
                    [ C.font_semibold ]
                    [ UI.Kit.inlineIcon Icons.warning
                    , text "This app requires a proper implementation of "
                    , UI.Kit.link
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
                , UI.Kit.link
                    { label = "this one"
                    , url = "https://github.com/hacdias/webdav"
                    }
                , text ", do. You can find the configuration for that server "
                , UI.Kit.link
                    { label = "here"
                    , url = "about#CORS__WebDAV"
                    }
                , text "."
                ]
