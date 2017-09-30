module Sources.View exposing (..)

import Color
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit, onWithOptions)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
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
import Navigation.View as Navigation
import Routing.Types
import Sources.Services as Services
import Sources.Types as Sources exposing (..)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- Styles

import Form.Styles as FormStyles
import List.Styles exposing (Classes(..))
import Navigation.Styles exposing (Classes(..))
import Styles exposing (Classes(..))


-- 🍯


entry : Sources.Page -> TopLevel.Model -> Html TopLevel.Msg
entry page model =
    case page of
        Edit _ ->
            lazy
                pageEdit
                model.sources.form

        Index ->
            lazy3
                pageIndex
                model.sources.collection
                model.sources.isProcessing
                model.sources.processingErrors

        New ->
            lazy
                pageNew
                model.sources.form



-- {Page} index


pageIndex : List Source -> IsProcessing -> List ( SourceId, String ) -> Html TopLevel.Msg
pageIndex sources isProcessing processingErrors =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            (List.concat
                [ -- Add
                  [ ( span
                        []
                        [ Icons.add colorDerivatives.text 16
                        , label [] [ text "Add a new source" ]
                        ]
                    , Sources.New
                        |> Routing.Types.Sources
                        |> Routing.Types.GoToPage
                        |> RoutingMsg
                    )
                  ]

                -- Other
                , if List.isEmpty sources then
                    []
                  else if Maybe.Extra.isJust isProcessing then
                    [ ( span
                            [ cssClass ActiveLink ]
                            [ Icons.sync colorDerivatives.text 16
                            , label [] [ text "Processing sources ..." ]
                            ]
                      , TopLevel.NoOp
                      )
                    ]
                  else
                    [ ( span
                            []
                            [ Icons.sync colorDerivatives.text 16
                            , label [] [ text "Process sources" ]
                            ]
                      , TopLevel.ProcessSources
                      )
                    ]
                ]
            )

        ------------------------------------
        -- List
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Sources" ]
            , if List.isEmpty sources then
                text ""
              else
                p
                    [ cssClass Intro ]
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
                                |> Maybe.andThen (List.find (Tuple.first >> .id >> (==) s.id))
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
                    div
                        [ cssClass EmptyState ]
                        [ Icons.add Color.black 16
                        , div
                            []
                            [ text "No sources have been added yet,"
                            , br [] []
                            , text "add one to get started."
                            ]
                        ]
                else
                    -- Render list
                    Html.Keyed.node
                        "ul"
                        [ cssClass ListWithActions ]
                        (List.indexedMap renderSource sourcesWithContext)
            ]
        ]


renderSource : Int -> ( Source, Bool, Maybe String ) -> ( String, Html TopLevel.Msg )
renderSource index ( source, isProcessing, processingError ) =
    let
        key =
            toString index
    in
        ( key
        , li
            [ rel key ]
            [ label
                []
                [ source.data
                    |> Dict.get "name"
                    |> Maybe.withDefault source.id
                    |> text
                ]
            , span
                [ cssClass ListActions ]
                [ -- Processing error
                  --
                  case processingError of
                    Just err ->
                        span [ title err ] [ Icons.error_outline colorDerivatives.error 16 ]

                    Nothing ->
                        text ""

                -- Is processing
                --
                , if isProcessing == True then
                    span [ title "Processing …" ] [ Icons.sync colorDerivatives.text 16 ]
                  else
                    text ""

                -- Enabled/Disabled
                --
                , a
                    [ source
                        |> ToggleSource
                        |> SourcesMsg
                        |> onClick
                    , if source.enabled then
                        title "Enabled (click to disable)"
                      else
                        title "Disabled (click to enable)"
                    ]
                    [ if source.enabled then
                        Icons.check colorDerivatives.text 16
                      else
                        Icons.not_interested colorDerivatives.text 16
                    ]

                -- Settings
                --
                , a
                    [ onWithOptions
                        "click"
                        { stopPropagation = True
                        , preventDefault = True
                        }
                        (Decode.map (TopLevel.ShowSourceMenu source.id) Mouse.position)
                    ]
                    [ Icons.settings colorDerivatives.text 16 ]
                ]
            ]
        )



-- {Page} New


pageNew : Sources.Form -> Html TopLevel.Msg
pageNew sForm =
    div
        [ cssClasses
            [ InsulationContent
            , InsulationFlexContent
            ]
        ]
        (case sForm of
            NewForm step source ->
                [ ------------------------------------
                  -- Navigation
                  ------------------------------------
                  Navigation.insideCustom
                    (case step of
                        1 ->
                            [ ( span
                                    []
                                    [ Icons.list colorDerivatives.text 16
                                    , label [] [ text "" ]
                                    ]
                              , Sources.Index
                                    |> Routing.Types.Sources
                                    |> Routing.Types.GoToPage
                                    |> RoutingMsg
                              )
                            ]

                        _ ->
                            [ ( span
                                    []
                                    [ Icons.arrow_back colorDerivatives.text 16
                                    , label [] [ text "Take a step back" ]
                                    ]
                              , SourcesMsg (AssignFormStep (step - 1))
                              )
                            ]
                    )

                ------------------------------------
                -- Form
                ------------------------------------
                , Html.map
                    SourcesMsg
                    (pageNewForm step source)
                ]

            _ ->
                [ text "Cannot use this model.form on this page" ]
        )


pageNewForm : Int -> Source -> Html Sources.Msg
pageNewForm step source =
    formNode
        (case step of
            1 ->
                Sources.AssignFormStep 2

            2 ->
                Sources.AssignFormStep 3

            3 ->
                Sources.SubmitForm

            _ ->
                Sources.AssignFormStep 1
        )
        (case step of
            1 ->
                pageNewStep1 source step

            2 ->
                pageNewStep2 source step

            3 ->
                pageNewStep3 source step

            _ ->
                text ""
        )


pageNewStep1 : Source -> Int -> Html Sources.Msg
pageNewStep1 source step =
    div
        []
        [ h2
            []
            [ text "Where is your music stored?" ]
        , div
            [ cssClass FormStyles.SelectBox
            , style
                [ ( "max-width", "350px" )
                , ( "width", "100%" )
                ]
            ]
            [ select
                [ onInput AssignFormService ]
                (List.map
                    (\( typStr, labe ) ->
                        option
                            [ selected <| (toString source.service) == typStr
                            , value typStr
                            ]
                            [ text labe ]
                    )
                    (Services.labels)
                )
            , Icons.expand_more (Color.greyscale 0.325) 20
            ]
        , button
            [ cssClass Button, type_ "submit" ]
            [ Icons.arrow_forward Color.black 16 ]
        ]


pageNewStep2 : Source -> Int -> Html Sources.Msg
pageNewStep2 source step =
    div
        []
        [ h3
            []
            [ text "Where exactly?" ]
        , div
            [ cssClasses
                [ Columns ]
            , style
                [ ( "text-align", "left" ) ]
            ]
            (renderSourceProperties source)
        , br
            []
            []
        , button
            [ cssClass Button, type_ "submit" ]
            [ Icons.arrow_forward Color.black 16 ]
        ]


pageNewStep3 : Source -> Int -> Html Sources.Msg
pageNewStep3 source step =
    div
        []
        [ h2
            []
            [ text "One last thing" ]
        , div
            [ style
                [ ( "margin", "0 auto" )
                , ( "max-width", "420px" )
                , ( "width", "100%" )
                ]
            ]
            [ label
                []
                [ text "What are we going to call this source?" ]
            , br
                []
                []
            , labelBox source
            ]
        , div
            [ cssClass Styles.Intro ]
            [ Icons.warning colorDerivatives.text 16
            , strong
                []
                [ text "Make sure CORS is enabled" ]
            , br
                []
                []
            , text "You can find the instructions over "
            , a
                [ href "/about#CORS"
                , target "blank"
                ]
                [ text "here" ]
            ]
        , button
            [ cssClass Button, type_ "submit" ]
            [ text "Add source" ]
        ]



-- {Page} Edit


pageEdit : Sources.Form -> Html TopLevel.Msg
pageEdit sForm =
    div
        [ cssClasses
            [ InsulationContent
            , InsulationFlexContent
            ]
        ]
        (case sForm of
            EditForm source ->
                [ ------------------------------------
                  -- Navigation
                  ------------------------------------
                  Navigation.insideCustom
                    [ ( span
                            []
                            [ Icons.list colorDerivatives.text 16
                            , label [] [ text "" ]
                            ]
                      , Sources.Index
                            |> Routing.Types.Sources
                            |> Routing.Types.GoToPage
                            |> RoutingMsg
                      )
                    ]

                ------------------------------------
                -- Form
                ------------------------------------
                , Html.map
                    SourcesMsg
                    (formNode
                        Sources.SubmitForm
                        (div
                            []
                            [ h3
                                []
                                [ text "Edit source" ]
                            , div
                                [ cssClasses
                                    [ Columns ]
                                , style
                                    [ ( "text-align", "left" ) ]
                                ]
                                (List.concat
                                    [ renderSourceProperties source
                                    , [ label
                                            []
                                            [ text "Name" ]
                                      , labelBox source
                                      ]
                                    ]
                                )
                            , br
                                []
                                []
                            , button
                                [ cssClass Button, type_ "submit" ]
                                [ text "Save" ]
                            ]
                        )
                    )
                ]

            _ ->
                [ text "Cannot use this model.form on this page" ]
        )



-- Forms


formNode : Sources.Msg -> Html Sources.Msg -> Html Sources.Msg
formNode submitMsg childNode =
    Html.form
        [ cssClasses
            [ InsulationContent
            , InsulationFlexContent
            , InsulationCentered
            ]
        , style
            [ ( "position", "relative" )
            , ( "text-align", "center" )
            ]
        , onSubmit submitMsg
        ]
        [ div
            [ cssClasses
                [ InsulationFlexContent ]
            , style
                [ ( "overflow", "hidden" )
                , ( "position", "relative" )
                , ( "width", "100%" )
                , ( "z-index", "9" )
                ]
            ]
            [ div
                [ cssClasses
                    [ InsulationContent
                    , InsulationCentered
                    ]
                ]
                [ div
                    [ cssClasses [ ContentBox ]
                    , style [ ( "padding-top", "2.25rem" ) ]
                    ]
                    [ childNode ]
                ]
            ]
        , div
            [ cssClass LogoBackdrop ]
            []
        ]



-- Properties


propertyRenderer : Source -> ( String, String, String, Bool ) -> Html Sources.Msg
propertyRenderer source ( propKey, propLabel, propPlaceholder, isPassword ) =
    div
        []
        [ label
            []
            [ text propLabel ]
        , div
            [ cssClass FormStyles.InputBox ]
            [ input
                [ name propKey
                , onInput (Sources.AssignFormProperty propKey)
                , placeholder propPlaceholder
                , required True
                , type_
                    (if isPassword then
                        "password"
                     else
                        "text"
                    )
                , value
                    (source.data
                        |> Dict.get propKey
                        |> Maybe.withDefault ""
                    )
                ]
                []
            ]
        ]


renderSourceProperties : Source -> List (Html Sources.Msg)
renderSourceProperties source =
    source.service
        |> Services.properties
        |> List.reverse
        |> List.drop 1
        |> List.reverse
        |> List.map (propertyRenderer source)


labelBox : Source -> Html Sources.Msg
labelBox source =
    div
        [ cssClass FormStyles.InputBox ]
        [ input
            [ name "name"
            , onInput (Sources.AssignFormProperty "name")
            , placeholder
                (source.service
                    |> Services.properties
                    |> List.reverse
                    |> List.head
                    |> Maybe.map (\( _, l, _, _ ) -> l)
                    |> Maybe.withDefault "Label"
                )
            , required True
            , type_ "text"
            , value
                (source.data
                    |> Dict.get "name"
                    |> Maybe.withDefault ""
                )
            ]
            []
        ]
