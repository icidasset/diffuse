module Queue.View exposing (..)

import Color
import Html exposing (..)
import Html.Attributes exposing (rel)
import Html.Events exposing (onClick)
import Html.Keyed
import Html.Lazy exposing (lazy, lazy2)
import Material.Icons.Av as Icons
import Material.Icons.Content as Icons
import Material.Icons.Image as Icons
import Navigation.View as Navigation
import Queue.Types as Queue exposing (Item, Page(..))
import Routing.Types
import Types as TopLevel exposing (Model, Msg(..))
import Utils exposing (cssClass)
import Variables exposing (colorDerivatives)


-- Styles

import List.Styles exposing (Classes(..))
import Styles exposing (Classes(..))


-- Helpers


type alias ItemWithActions =
    ( Item, List (Html Msg) )



-- 🍯


entry : Queue.Page -> TopLevel.Model -> Html TopLevel.Msg
entry page model =
    case page of
        Index ->
            lazy2 pageIndex model.queue.future model.queue.shuffle

        History ->
            lazy pageHistory model.queue.past



-- {Page} index


pageIndex : List Item -> Bool -> Html TopLevel.Msg
pageIndex futureItems shuffled =
    div
        [ cssClass InsulationContent ]
        [ ------------------------------------
          -- Navigation
          ------------------------------------
          Navigation.insideCustom
            [ ( span
                    []
                    [ Icons.queue_music colorDerivatives.text 16
                    , label [] [ text "History" ]
                    ]
              , RoutingMsg (Routing.Types.GoToUrl "/queue/history")
              )
            , ( span
                    []
                    [ Icons.clear colorDerivatives.text 16
                    , label [] [ text "Clear" ]
                    ]
              , QueueMsg (Queue.Clear)
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
                div
                    [ cssClass EmptyState ]
                    [ Icons.music_note Color.black 16
                    , div
                        []
                        [ text "Nothing here yet,"
                        , br [] []
                        , text "add some music first."
                        ]
                    ]
              else
                Html.Keyed.node
                    "ul"
                    [ cssClass ListWithActions ]
                    (futureItems
                        |> List.indexedMap (futureActions shuffled)
                        |> List.indexedMap renderItem
                    )
            ]
        ]


futureActions : Bool -> Int -> Item -> ItemWithActions
futureActions shuffled index item =
    (,)
        item
        [ if item.manualEntry || shuffled then
            a
                [ index
                    |> Queue.RemoveItem
                    |> TopLevel.QueueMsg
                    |> onClick
                ]
                [ Icons.remove_circle_outline (Color.grayscale 0.175) 16 ]
          else
            text ""
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
                div
                    [ cssClass EmptyState ]
                    [ Icons.music_note Color.black 16
                    , div
                        []
                        [ text "Nothing here yet,"
                        , br [] []
                        , text "play some music first."
                        ]
                    ]
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
                    span [] [ text itemLabel ]
                  else
                    span [ cssClass SubtleListItem ] [ text itemLabel ]
                ]
            , span
                [ cssClass ListActions ]
                (actions)
            ]
        )
