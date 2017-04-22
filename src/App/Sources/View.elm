module Sources.View exposing (..)

import Color
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy3)
import List.Extra as List
import Material.Icons.Alert as Icons
import Material.Icons.Action as Icons
import Material.Icons.File as Icons
import Material.Icons.Content as Icons
import Material.Icons.Navigation as Icons
import Material.Icons.Notification as Icons
import Navigation.View as Navigation
import Routing.Types exposing (Msg(..))
import Sources.Types as Sources exposing (..)
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colorDerivatives)


-- Styles

import Form.Styles as FormStyles
import List.Styles exposing (Classes(..))
import Styles exposing (Classes(Button, ContentBox, InsulationContent, Intro))


-- Services

import Sources.Services.AmazonS3 as AmazonS3


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
            lazy pageNew model.sources.newSource



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
                  case processingError of
                    Just err ->
                        span [ title err ] [ Icons.error_outline colorDerivatives.error 16 ]

                    Nothing ->
                        text ""

                -- Is processing
                , if isProcessing == True then
                    span [ title "Processing …" ] [ Icons.sync colorDerivatives.text 16 ]
                  else
                    text ""

                -- Delete
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


pageNew : Source -> Html TopLevel.Msg
pageNew newSource =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( span
                    []
                    [ Icons.list colorDerivatives.text 16
                    , label [] [ text "Go to index" ]
                    ]
              , "/sources"
              )
            ]

        ------------------------------------
        -- Form
        ------------------------------------
        , Html.map SourcesMsg (pageNewForm newSource)
        ]


pageNewForm : Source -> Html Sources.Msg
pageNewForm newSource =
    Html.form
        [ cssClass ContentBox
        , onSubmit Sources.SubmitNewSourceForm
        ]
        [ h1
            []
            [ text "Add a new source" ]
        , p
            [ cssClass Styles.Intro ]
            [ text """
                Choose a type of source, fill in its credentials and add it.
              """
            ]
        , label
            []
            [ text "Source type/service" ]
        , div
            [ cssClass FormStyles.SelectBox ]
            [ select
                []
                [ option
                    [ value "amazon-s3" ]
                    [ text "Amazon S3" ]
                ]
            , Icons.expand_more (Color.greyscale 0.325) 20
            ]
        , div
            []
            (renderSourceProperties newSource)
        , div
            []
            [ button
                [ type_ "submit" ]
                [ text "Create source" ]
            ]
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
                , onInput (Sources.SetNewSourceProperty source propKey)
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
    case source.service of
        AmazonS3 ->
            List.map (propertyRenderer source) AmazonS3.properties
