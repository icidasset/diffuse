module Sources.View exposing (..)

import Form.Styles as FormStyles
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Navigation.View as Navigation
import Routing.Types as Routing
import Sources.Types as Sources exposing (Page(..), Source(..))
import Styles exposing (Classes(Button, ContentBox))
import Types exposing (Model, Msg(..))
import Utils exposing (cssClass)


-- Services

import Sources.Services.AmazonS3


-- 🍯


entry : Sources.Page -> Model -> Html Msg
entry page model =
    case page of
        Index ->
            pageIndex model

        New ->
            pageNew model



-- {Page} index


pageIndex : Model -> Html Msg
pageIndex _ =
    div
        []
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.horizontal
            [ ( "Add a new source", "/sources/new" )
            ]
        ]



-- {Page} New


pageNew : Model -> Html Msg
pageNew model =
    div
        []
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.horizontal
            [ ( "Show my sources", "/sources" )
            ]
          ------------------------------------
          -- Form
          ------------------------------------
        , Html.map SourcesMsg (pageNewForm model)
        ]


pageNewForm : Model -> Html Sources.Msg
pageNewForm model =
    Html.form
        [ cssClass ContentBox
        , onSubmit Sources.SubmitNewSourceForm
        ]
        [ h1
            []
            [ text "Add a new source" ]
        , p
            [ cssClass FormStyles.Intro ]
            [ text """
                A source is a place where your music is stored.
                By connecting a source, the application will scan it
                and keep a list of all the music in it. It will not
                copy anything.
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
            ]
        , div
            []
            (renderSourceProperties model.sources.newSource)
        , div
            []
            [ button
                [ type_ "submit" ]
                [ text "Create source" ]
            ]
        ]



-- Properties


propertyRenderer :
    Source
    -> (String -> String)
    -> ( String, String, String, Bool )
    -> Html Sources.Msg
propertyRenderer source translator ( propKey, propLabel, propPlaceholder, isPassword ) =
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
                , value (translator propKey)
                ]
                []
            ]
        ]


renderSourceProperties : Source -> List (Html Sources.Msg)
renderSourceProperties source =
    let
        ( translator, properties ) =
            case source of
                AmazonS3 data ->
                    ( Sources.Services.AmazonS3.translateFrom data
                    , Sources.Services.AmazonS3.properties
                    )
    in
        List.map (propertyRenderer source translator) properties
