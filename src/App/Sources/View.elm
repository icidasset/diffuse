module Sources.View exposing (..)

import Form.Styles as FormStyles
import HorizontalNavigation.View as HorizontalNavigation
import Html exposing (..)
import Html.Attributes exposing (..)
import Routing.Types as Routing
import Sources.Types as Sources exposing (Page(..), Source(..))
import Styles exposing (Classes(ContentBox))
import Types exposing (Model, Msg)
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
          HorizontalNavigation.entry
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
          HorizontalNavigation.entry
            [ ( "Show my sources", "/sources" )
            ]
          ------------------------------------
          -- Form
          ------------------------------------
        , Html.form
            [ cssClass ContentBox ]
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
            ]
        ]



-- Properties


propertyRenderer :
    source
    -> (source -> String -> Maybe String)
    -> ( String, String, String )
    -> Html msg
propertyRenderer source translator ( propKey, propLabel, propPlaceholder ) =
    div
        []
        [ label
            []
            [ text propLabel ]
        , div
            [ cssClass FormStyles.InputBox ]
            [ input
                [ name propKey
                , type_ "text"
                , placeholder propPlaceholder
                , value
                    (propKey
                        |> translator source
                        |> Maybe.withDefault ""
                    )
                ]
                []
            ]
        ]


renderSourceProperties : Source -> List (Html msg)
renderSourceProperties source =
    let
        ( src, translator, properties ) =
            case source of
                AmazonS3 src ->
                    ( src
                    , Sources.Services.AmazonS3.translator
                    , Sources.Services.AmazonS3.properties
                    )
    in
        List.map (propertyRenderer src translator) properties
