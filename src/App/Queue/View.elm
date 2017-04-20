module Queue.View exposing (..)

import Color
import Html exposing (..)
import Html.Attributes exposing (rel)
import Html.Events exposing (onClick)
import Html.Keyed
import Html.Lazy exposing (lazy)
import Material.Icons.Content as Icons
import Material.Icons.Av as Icons
import Navigation.View as Navigation
import Queue.Types as Queue exposing (Item, Page(..))
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colorDerivatives)


-- Styles

import List.Styles exposing (Classes(..))
import Styles exposing (Classes(Button, ContentBox, InsulationContent, Intro))


-- Helpers


type alias ItemWithActions =
    ( Item, List (Html Msg) )



-- 🍯


entry : Queue.Page -> TopLevel.Model -> Html TopLevel.Msg
entry page model =
    case page of
        Index ->
            lazy pageIndex model.queue.future

        History ->
            lazy pageHistory model.queue.past



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
            , if List.isEmpty futureItems then
                p
                    [ cssClass Intro ]
                    [ text "No tracks available." ]
              else
                Html.Keyed.node
                    "ul"
                    [ cssClass ListWithActions ]
                    (futureItems
                        |> List.indexedMap futureActions
                        |> List.indexedMap renderItem
                    )
            ]
        ]


futureActions : Int -> Item -> ItemWithActions
futureActions index item =
    (,)
        item
        [ a
            [ index
                |> Queue.RemoveItem
                |> TopLevel.QueueMsg
                |> onClick
            ]
            [ Icons.remove_circle_outline (Color.grayscale 0.175) 16 ]
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
            , if List.isEmpty pastItems then
                p
                    [ cssClass Intro ]
                    [ text "No tracks have been played yet." ]
              else
                Html.Keyed.node
                    "ul"
                    [ cssClass ListWithActions ]
                    (pastItems
                        |> List.reverse
                        |> List.map (\item -> ( item, [] ))
                        |> List.indexedMap renderItem
                    )
            ]
        ]



-- Child views


renderItem : Int -> ItemWithActions -> ( String, Html TopLevel.Msg )
renderItem index ( item, actions ) =
    let
        key =
            toString index

        itemLabel =
            item.track.tags.artist ++ " – " ++ item.track.tags.title
    in
        ( key
        , li
            [ rel key ]
            [ label
                []
                [ small [] [ text (toString (index + 1) ++ ".") ]
                , if item.manualEntry == True then
                    strong [] [ text itemLabel ]
                  else
                    span [] [ text itemLabel ]
                ]
            , span
                [ cssClass ListActions ]
                (actions)
            ]
        )
