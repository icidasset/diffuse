module Queue.View exposing (..)

import Html exposing (Html, div, h1, label, li, span, text, ul)
import Html.Attributes exposing (rel)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Material.Icons.Av as Icons
import Navigation.View as Navigation
import Queue.Types as Queue exposing (Item, Page(..))
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colorDerivatives)


-- Styles

import List.Styles exposing (Classes(..))
import Styles exposing (Classes(Button, ContentBox, InsulationContent))


-- 🍯


entry : Queue.Page -> TopLevel.Model -> Html TopLevel.Msg
entry page model =
    case page of
        Index ->
            lazy pageIndex model.queue.future

        History ->
            lazy pageIndex model.queue.past



-- {Page} index


pageIndex : List Item -> Html TopLevel.Msg
pageIndex futureItems =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( span
                    []
                    [ Icons.queue_music colorDerivatives.text 16
                    , label [] [ text "History" ]
                    ]
              , "/queue/history"
              )
            ]

        ------------------------------------
        -- List
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "Up next" ]
            , Html.Keyed.node
                "ul"
                [ cssClass ListWithActions ]
                (List.indexedMap renderItem futureItems)
            ]
        ]



-- {Page} history


pageHistory : List Item -> Html TopLevel.Msg
pageHistory pastItems =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.inside
            [ ( span
                    []
                    [ Icons.queue_music colorDerivatives.text 16
                    , label [] [ text "Up next" ]
                    ]
              , "/queue"
              )
            ]

        ------------------------------------
        -- List
        ------------------------------------
        , div
            [ cssClass ContentBox ]
            [ h1
                []
                [ text "History" ]
            , Html.Keyed.node
                "ul"
                [ cssClass ListWithActions ]
                (List.indexedMap renderItem pastItems)
            ]
        ]



-- Child views


renderItem : Int -> Item -> ( String, Html TopLevel.Msg )
renderItem index item =
    let
        key =
            toString index
    in
        ( key
        , li
            [ rel key ]
            [ text item.track.tags.artist
            , text " - "
            , text item.track.tags.title
            ]
        )
