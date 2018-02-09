module Sources.View exposing (..)

import Color
import Color.Convert
import Dict
import Html
import Html.Attributes
import Json.Decode as Decode
import List.Extra as List
import Material.Icons.Alert as Icons
import Material.Icons.Action as Icons
import Material.Icons.Av as Icons
import Material.Icons.File as Icons
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Maybe.Extra
import Mouse
import Navigation.Types exposing (..)
import Navigation.View as Navigation
import Routing.Types exposing (Msg(RedirectTo))
import Sources.Services as Services
import Sources.Services.Dropbox as Dropbox
import Sources.Types as Sources exposing (..)
import Sources.Utils exposing (isViable)
import Types as TopLevel exposing (Msg(..))


-- Elements

import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Events exposing (onClick, onInput, onWithOptions)
import Element.Ext exposing (..)
import Element.Keyed
import Element.Input as Input
import Element.Types exposing (Node)
import Layouts exposing (..)
import Variables exposing (colorDerivatives, colors, scaled)
import Variations exposing (Variations(..))


-- Styles

import Form.Styles exposing (Styles(Input))
import List.Styles exposing (Styles(..))
import Styles exposing (Styles(..))


-- 🍯


entry : Sources.Page -> TopLevel.Model -> Node
entry page model =
    case page of
        Edit _ ->
            lazySpread
                pageEdit
                model.sources.form

        Index ->
            lazySpread3
                pageIndex
                model.sources.collection
                ( model.sources.isProcessing, model.sources.processingErrors )
                { isElectron = model.isElectron }

        New ->
            lazySpread3
                pageNew
                model.sources.form
                model.origin
                model.isElectron

        NewThroughRedirect service hash ->
            lazySpread3
                pageNew
                model.sources.form
                model.origin
                model.isElectron



-- {Page} Index


pageIndex :
    List Source
    -> ( IsProcessing, List ( SourceId, String ) )
    -> ViabilityDependencies
    -> Node
pageIndex sources ( isProcessing, processingErrors ) viabilityDependencies =
    column
        Zed
        [ height fill ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom <|
            (++)
                -- Add
                --
                [ ( Icon Icons.add
                  , Label (Shown "Add a new source")
                    --
                  , Sources.New
                        |> Routing.Types.Sources
                        |> Routing.Types.GoToPage
                        |> RoutingMsg
                  )
                ]
                -- Other
                --
                (if List.isEmpty sources then
                    []
                 else if Maybe.Extra.isJust isProcessing then
                    [ ( Icon Icons.sync
                      , Label (Shown "Processing sources ...")
                      , TopLevel.NoOp
                      )
                    ]
                 else
                    [ ( Icon Icons.sync
                      , Label (Shown "Process sources")
                      , TopLevel.ProcessSources
                      )
                    ]
                )

        ------------------------------------
        -- List
        ------------------------------------
        , column
            Zed
            [ height fill
            , paddingXY (scaled 4) 0
            ]
            [ Layouts.h1 "Sources"

            -- Lists
            --
            , if List.isEmpty sources then
                empty
              else
                Layouts.intro
                    [ text """
                        A source is a place where your music is stored.
                        By connecting a source, the application will scan it
                        and keep a list of all the music in it. It will not
                        copy anything.
                      """
                    ]

            -- Check if sources are processing
            -- and if they have processing errors
            , let
                sourcesWithContext =
                    List.map
                        (\s ->
                            ( s
                            , s
                                |> isViable viabilityDependencies
                            , isProcessing
                                |> Maybe.andThen (List.find (.id >> (==) s.id))
                                |> Maybe.map (always True)
                                |> Maybe.withDefault False
                            , processingErrors
                                |> List.find (Tuple.first >> (==) s.id)
                                |> Maybe.map (Tuple.second)
                            )
                        )
                        sources
              in
                if List.isEmpty sources then
                    -- No sources atm
                    Layouts.emptyState
                        Icons.add
                        [ text "No sources have been added yet,"
                        , text "add one to get started."
                        ]
                else
                    -- Render list
                    Element.Keyed.column
                        (List Container)
                        []
                        (List.indexedMap renderSource sourcesWithContext)
            ]
        ]


renderSource : Int -> ( Source, Bool, Bool, Maybe String ) -> ( String, Node )
renderSource index ( source, sourceIsViable, isProcessing, processingError ) =
    let
        key =
            toString index
    in
        ( key
        , listItem
            [ attribute "rel" key
            , inlineStyle
                (if sourceIsViable then
                    []
                 else
                    [ ( "color", Color.Convert.colorToCssRgb colors.base04 ) ]
                )
            ]
            [ el
                Zed
                [ width fill ]
                (source.data
                    |> Dict.get "name"
                    |> Maybe.withDefault source.id
                    |> text
                )
            , listItemActions
                [ -- Processing error
                  --
                  case processingError of
                    Just err ->
                        el
                            WithoutLineHeight
                            [ attribute "title" err ]
                            (16 |> Icons.error_outline colorDerivatives.error |> html)

                    Nothing ->
                        empty

                -- Is processing
                --
                , if isProcessing == True then
                    el
                        WithoutLineHeight
                        [ attribute "title" "Processing …" ]
                        (16 |> Icons.sync colorDerivatives.text |> html)
                  else
                    empty

                -- Enabled/Disabled
                --
                , if not sourceIsViable then
                    el
                        WithoutLineHeight
                        [ attribute "title" "Disabled (not available on this platform)" ]
                        (16
                            |> Icons.not_interested colors.base04
                            |> html
                        )
                  else
                    el
                        WithoutLineHeight
                        [ source
                            |> ToggleSource
                            |> SourcesMsg
                            |> onClick

                        --
                        , if source.enabled then
                            attribute "title" "Enabled (click to disable)"
                          else
                            attribute "title" "Disabled (click to enable)"
                        ]
                        (if source.enabled then
                            html (Icons.check colorDerivatives.text 16)
                         else
                            html (Icons.not_interested colorDerivatives.text 16)
                        )

                -- Settings
                --
                , el
                    WithoutLineHeight
                    [ Mouse.position
                        |> Decode.map (TopLevel.ShowSourceMenu source.id)
                        |> onWithOptions
                            "click"
                            { stopPropagation = True
                            , preventDefault = True
                            }
                    ]
                    (16
                        |> Icons.settings colorDerivatives.text
                        |> html
                    )
                ]
            ]
        )



-- {Page} New


pageNew : Sources.Form -> String -> Bool -> Node
pageNew sForm origin isElectron =
    column
        Zed
        [ height fill ]
        (case sForm of
            NewForm step source ->
                [ ------------------------------------
                  -- Navigation
                  ------------------------------------
                  Navigation.insideCustom
                    (case step of
                        1 ->
                            [ ( Icon Icons.arrow_back
                              , Label (Hidden "Go back")
                                --
                              , Sources.Index
                                    |> Routing.Types.Sources
                                    |> Routing.Types.GoToPage
                                    |> RoutingMsg
                              )
                            ]

                        _ ->
                            let
                                newStep =
                                    case source.service of
                                        Local ->
                                            1

                                        _ ->
                                            step - 1
                            in
                                [ ( Icon Icons.arrow_back
                                  , Label (Shown "Take a step back")
                                    --
                                  , SourcesMsg (AssignFormStep newStep)
                                  )
                                ]
                    )

                ------------------------------------
                -- Form
                ------------------------------------
                , within
                    [ logoBackdrop, takeOver (pageNewForm step source origin isElectron) ]
                    (takeOver empty)
                ]

            _ ->
                [ text "Cannot use this model.form on this page" ]
        )


pageNewForm : Int -> Source -> String -> Bool -> Node
pageNewForm step source origin isElectron =
    case step of
        1 ->
            pageNewStep1 source isElectron

        2 ->
            pageNewStep2 source origin

        3 ->
            pageNewStep3 source

        _ ->
            empty


pageNewStep1 : Source -> Bool -> Node
pageNewStep1 source isElectron =
    let
        msg =
            SourcesMsg (Sources.AssignFormStep 2)
    in
        column Zed
            [ center
            , height fill
            , onEnterKey msg
            , paddingXY (scaled 4) 0
            , spacing (scaled 8)
            , verticalCenter
            , width fill
            ]
            [ h2 H2 [] (text "Where is your music stored?")

            --
            , Services.labels isElectron
                |> select (AssignFormService >> SourcesMsg) (toString source.service)
                |> el Zed [ maxWidth (px 350), width fill ]

            --
            , btn
                Button
                [ case source.service of
                    Local ->
                        onClick (SourcesMsg RequestLocalPath)

                    _ ->
                        onClick msg
                ]
                (18
                    |> Icons.arrow_forward colorDerivatives.success
                    |> html
                    |> el WithoutLineHeight [ padding (scaled -15) ]
                )
            ]


pageNewStep2 : Source -> String -> Node
pageNewStep2 source origin =
    let
        msg =
            case source.service of
                Dropbox ->
                    RoutingMsg (RedirectTo <| Dropbox.authorizationUrl origin source.data)

                _ ->
                    SourcesMsg (Sources.AssignFormStep 3)
    in
        column Zed
            [ height fill
            , onEnterKey msg
            , paddingXY (scaled 4) 0
            , spacing (scaled 8)
            , verticalCenter
            , width fill
            ]
            [ h3
                H3
                []
                (text "Where exactly?")

            --
            , paragraph
                Columns
                [ width fill ]
                (renderSourceProperties source)

            --
            , btn
                Button
                [ center, onClick msg ]
                (18
                    |> Icons.arrow_forward colorDerivatives.success
                    |> html
                    |> el WithoutLineHeight [ padding (scaled -15) ]
                )
            ]


pageNewStep3 : Source -> Node
pageNewStep3 source =
    let
        msg =
            SourcesMsg Sources.SubmitForm
    in
        column Zed
            [ center
            , height fill
            , onEnterKey msg
            , paddingXY (scaled 4) 0
            , spacing (scaled 6)
            , verticalCenter
            , width fill
            ]
            [ h2 H2 [] (text "One last thing")

            --
            , lbl "What are we going to call this source?"

            --
            , Input.text
                (Form Input)
                [ center
                , inputBottomPadding
                , inputTopPadding
                , maxWidth (px 420)
                , width fill
                ]
                { onChange =
                    SourcesMsg << Sources.AssignFormProperty "name"
                , value =
                    source.data
                        |> Dict.get "name"
                        |> Maybe.withDefault ""
                , label =
                    Input.placeholder
                        { text =
                            source.service
                                |> Services.properties
                                |> List.reverse
                                |> List.head
                                |> Maybe.map (\( _, l, _, _ ) -> l)
                                |> Maybe.withDefault "Label"
                        , label =
                            Input.hiddenLabel "name"
                        }
                , options = []
                }

            --
            , paragraph
                Intro
                [ inlineStyle
                    [ ( "text-align", "center" ) ]
                , paddingBottom (scaled -4)
                , paddingTop (scaled 4)
                ]
                [ 14
                    |> Icons.warning colorDerivatives.text
                    |> html
                    |> el Zed [ moveDown 2, paddingRight (scaled -8) ]

                --
                , bold "Make sure CORS is enabled"
                , lineBreak
                , text "You can find the instructions over "
                , html
                    (Html.a
                        [ Html.Attributes.class "is-styled-link"
                        , Html.Attributes.href "/about#CORS"
                        , Html.Attributes.target "_blank"
                        ]
                        [ Html.text "here" ]
                    )
                ]

            --
            , btn
                Button
                [ onClick msg ]
                (text "Add source")
            ]



-- {Page} Edit


pageEdit : Sources.Form -> Node
pageEdit sForm =
    column
        Zed
        [ height fill ]
        (case sForm of
            EditForm source ->
                [ ------------------------------------
                  -- Navigation
                  ------------------------------------
                  Navigation.insideCustom
                    [ ( Icon Icons.arrow_back
                      , Label (Hidden "Go back")
                        --
                      , Sources.Index
                            |> Routing.Types.Sources
                            |> Routing.Types.GoToPage
                            |> RoutingMsg
                      )
                    ]

                ------------------------------------
                -- Form
                ------------------------------------
                , within
                    [ logoBackdrop, takeOver (pageEditForm source) ]
                    (takeOver empty)
                ]

            _ ->
                [ text "Cannot use this model.form on this page" ]
        )


pageEditForm : Source -> Node
pageEditForm source =
    let
        msg =
            SourcesMsg Sources.SubmitForm
    in
        column Zed
            [ height fill
            , onEnterKey msg
            , paddingXY (scaled 4) 0
            , spacing (scaled 8)
            , verticalCenter
            , width fill
            ]
            [ h3
                H3
                []
                (text "Edit source")

            --
            , paragraph
                Columns
                [ width fill ]
                (renderSourceProperties source)

            --
            , btn
                Button
                [ center, onClick msg ]
                (text "Save")
            ]



-- Properties


propertyRenderer : Source -> ( String, String, String, Bool ) -> Node
propertyRenderer source ( propKey, propLabel, propPlaceholder, isPassword ) =
    let
        input =
            if isPassword then
                Input.newPassword
            else
                Input.text
    in
        [ lbl propLabel

        --
        , input
            (Form Input)
            [ center
            , inputBottomPadding
            , inputTopPadding
            , maxWidth (px 420)
            , width fill
            ]
            { onChange =
                SourcesMsg << Sources.AssignFormProperty propKey
            , value =
                source.data
                    |> Dict.get propKey
                    |> Maybe.withDefault ""
            , label =
                Input.placeholder
                    { text = propPlaceholder
                    , label = Input.hiddenLabel propKey
                    }
            , options = []
            }
        ]
            |> column Zed [ inlineStyle [ ( "display", "block" ) ] ]
            |> el ColumnsChild [ paddingXY 0 (scaled 1) ]


renderSourceProperties : Source -> List Node
renderSourceProperties source =
    source.service
        |> Services.properties
        |> List.filter (\( k, _, _, _ ) -> List.notMember k sourcePropertiesToIgnore)
        |> List.map (propertyRenderer source)


sourcePropertiesToIgnore : List String
sourcePropertiesToIgnore =
    [ "accessToken", "name" ]
