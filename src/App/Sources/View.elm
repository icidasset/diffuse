module Sources.View exposing (..)

import Color
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import List.Extra as List
import Material.Icons.Alert as Icons
import Material.Icons.Action as Icons
import Material.Icons.Av as Icons
import Material.Icons.File as Icons
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Navigation.View as Navigation
import Routing.Types exposing (Msg(..))
import Sources.Services as Services
import Sources.Types as Sources exposing (..)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (..)
import Variables exposing (colorDerivatives)


-- Styles

import Form.Styles as FormStyles
import List.Styles exposing (Classes(..))
import Styles exposing (Classes(..))


-- 🍯


entry : Sources.Page -> TopLevel.Model -> Html TopLevel.Msg
entry page model =
    case page of
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
            [ ( span
                    []
                    [ Icons.add_circle_outline colorDerivatives.text 16
                    , label [] [ text "Add a new source" ]
                    ]
              , RoutingMsg (GoToUrl "/sources/new")
              )
            , ( span
                    []
                    [ Icons.cloud_queue colorDerivatives.text 16
                    , label [] [ text "Process sources" ]
                    ]
              , TopLevel.ProcessSources
              )
            ]

        ------------------------------------
        -- List
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Sources" ]
            , p
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
                        Icons.check_circle colorDerivatives.text 16
                      else
                        Icons.not_interested colorDerivatives.text 16
                    ]

                -- Delete
                --
                , a
                    [ title "Remove"
                    , source.id
                        |> Sources.Destroy
                        |> TopLevel.SourcesMsg
                        |> onClick
                    ]
                    [ Icons.remove_circle_outline colorDerivatives.text 16 ]
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
                              , RoutingMsg (GoToUrl "/sources")
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
        , onSubmit
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
                    [ cssClasses
                        [ ContentBox ]
                    , style
                        [ ( "padding-top", "2.25rem" ) ]
                    ]
                    [ case step of
                        1 ->
                            pageNewStep1 source step

                        2 ->
                            pageNewStep2 source step

                        3 ->
                            pageNewStep3 source step

                        _ ->
                            text ""
                    ]
                ]
            ]
        , div
            [ cssClass LogoBackdrop ]
            []
        ]


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
            , div
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
                [ href "https://gist.github.com/icidasset/c1883d594574a958ae4b4a5a91db1070#cors"
                , target "blank"
                ]
                [ text "here" ]
            ]
        , button
            [ cssClass Button, type_ "submit" ]
            [ text "Add source" ]
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
