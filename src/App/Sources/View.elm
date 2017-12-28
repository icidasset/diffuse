module Sources.View exposing (..)

import Color
import Dict
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
import Routing.Types
import Sources.Services as Services
import Sources.Types as Sources exposing (..)
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
            -- lazy
            --     pageEdit
            --     model.sources.form
            empty

        Index ->
            -- TODO: Use Element.Lazy once it's available
            pageIndex
                model.sources.collection
                model.sources.isProcessing
                model.sources.processingErrors

        New ->
            -- TODO: Use Element.Lazy once it's available
            pageNew
                model.sources.form



-- {Page} index


pageIndex : List Source -> IsProcessing -> List ( SourceId, String ) -> Node
pageIndex sources isProcessing processingErrors =
    column
        Zed
        [ height fill ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustomNew <|
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


renderSource : Int -> ( Source, Bool, Maybe String ) -> ( String, Node )
renderSource index ( source, isProcessing, processingError ) =
    let
        key =
            toString index
    in
        ( key
        , listItem
            [ attribute "rel" key ]
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
                , el
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


pageNew : Sources.Form -> Node
pageNew sForm =
    column
        Zed
        [ height fill ]
        (case sForm of
            NewForm step source ->
                [ ------------------------------------
                  -- Navigation
                  ------------------------------------
                  Navigation.insideCustomNew
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
                            [ ( Icon Icons.arrow_back
                              , Label (Shown "Take a step back")
                                --
                              , SourcesMsg (AssignFormStep (step - 1))
                              )
                            ]
                    )

                ------------------------------------
                -- Form
                ------------------------------------
                , within
                    [ logoBackdrop, takeOver (pageNewForm step source) ]
                    (takeOver empty)
                ]

            _ ->
                [ text "Cannot use this model.form on this page" ]
        )


pageNewForm : Int -> Source -> Node
pageNewForm step source =
    case step of
        1 ->
            pageNewStep1 source

        2 ->
            pageNewStep2 source

        -- 3 ->
        --     pageNewStep3 source
        _ ->
            empty


pageNewStep1 : Source -> Node
pageNewStep1 source =
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
            , Services.labels
                |> select (AssignFormService >> SourcesMsg) (toString source.service)
                |> el Zed [ maxWidth (px 350), width fill ]

            --
            , btn
                Button
                [ onClick msg ]
                (18
                    |> Icons.arrow_forward colorDerivatives.success
                    |> html
                    |> el WithoutLineHeight [ padding (scaled -15) ]
                )
            ]


pageNewStep2 : Source -> Node
pageNewStep2 source =
    let
        msg =
            SourcesMsg (Sources.AssignFormStep 3)
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
            [ h3
                H2
                []
                (text "Where exactly?")

            --
            , column
                Columns
                [ inlineStyle [ ( "display", "block" ) ]
                , spacing (scaled 10)
                , width fill
                ]
                (renderSourceProperties source)

            --
            , btn
                Button
                [ onClick msg ]
                (18
                    |> Icons.arrow_forward colorDerivatives.success
                    |> html
                    |> el WithoutLineHeight [ padding (scaled -15) ]
                )
            ]



-- pageNewStep3 : Source -> Html Sources.Msg
-- pageNewStep3 source =
--     div
--         []
--         [ h2
--             []
--             [ text "One last thing" ]
--         , div
--             [ style
--                 [ ( "margin", "0 auto" )
--                 , ( "max-width", "420px" )
--                 , ( "width", "100%" )
--                 ]
--             ]
--             [ label
--                 []
--                 [ text "What are we going to call this source?" ]
--             , br
--                 []
--                 []
--             , labelBox source
--             ]
--         , div
--             [ cssClass StylesOld.Intro ]
--             [ Icons.warning colorDerivatives.text 16
--             , strong
--                 []
--                 [ text "Make sure CORS is enabled" ]
--             , br
--                 []
--                 []
--             , text "You can find the instructions over "
--             , a
--                 [ href "/about#CORS"
--                 , target "blank"
--                 ]
--                 [ text "here" ]
--             ]
--         , button
--             [ cssClass Button, type_ "submit" ]
--             [ text "Add source" ]
--         ]
--
--
-- *** {Page} Edit
--
--
-- pageEdit : Sources.Form -> Html TopLevel.Msg
-- pageEdit sForm =
--     div
--         [ cssClasses
--             [ InsulationContent
--             , InsulationFlexContent
--             ]
--         ]
--         (case sForm of
--             EditForm source ->
--                 [ ------------------------------------
--                   -- Navigation
--                   ------------------------------------
--                   Navigation.insideCustom
--                     [ ( Icon Icons.arrow_back
--                       , Label (Hidden "Go back")
--                         --
--                       , Sources.Index
--                             |> Routing.Types.Sources
--                             |> Routing.Types.GoToPage
--                             |> RoutingMsg
--                       )
--                     ]
--
--                 ------------------------------------
--                 -- Form
--                 ------------------------------------
--                 , Html.map
--                     SourcesMsg
--                     (centeredForm
--                         Sources.SubmitForm
--                         (div
--                             []
--                             [ h3
--                                 []
--                                 [ text "Edit source" ]
--                             , div
--                                 [ cssClasses
--                                     [ Columns ]
--                                 , style
--                                     [ ( "text-align", "left" ) ]
--                                 ]
--                                 (List.concat
--                                     [ renderSourceProperties source
--                                     , [ label
--                                             []
--                                             [ text "Name" ]
--                                       , labelBox source
--                                       ]
--                                     ]
--                                 )
--                             , br
--                                 []
--                                 []
--                             , button
--                                 [ cssClass Button, type_ "submit" ]
--                                 [ text "Save" ]
--                             ]
--                         )
--                     )
--                 ]
--
--             _ ->
--                 [ text "Cannot use this model.form on this page" ]
--         )
--
--
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
        , Input.text
            (Form Input)
            [ center
            , maxWidth (px 420)
            , paddingXY 0 (scaled -5)
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
            |> column Zed []
            |> el ColumnsChild []


renderSourceProperties : Source -> List Node
renderSourceProperties source =
    source.service
        |> Services.properties
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> List.map (propertyRenderer source)



-- labelBox : Source -> Html Sources.Msg
-- labelBox source =
--     div
--         [ cssClass FormStyles.InputBox ]
--         [ input
--             [ name "name"
--             , onInput (Sources.AssignFormProperty "name")
--             , placeholder
--                 (source.service
--                     |> Services.properties
--                     |> List.reverse
--                     |> List.head
--                     |> Maybe.map (\( _, l, _, _ ) -> l)
--                     |> Maybe.withDefault "Label"
--                 )
--             , required True
--             , type_ "text"
--             , value
--                 (source.data
--                     |> Dict.get "name"
--                     |> Maybe.withDefault ""
--                 )
--             ]
--             []
--         ]
