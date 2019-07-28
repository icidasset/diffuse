module UI.Tracks.Scene.List exposing (Model, Msg(..), containerId, initialModel, scrollToNowPlaying, scrollToTop, update, view)

import Browser.Dom as Dom
import Chunky exposing (..)
import Classes as C
import Color
import Color.Ext as Color
import Color.Manipulate as Color
import Conditional exposing (ifThenElse)
import Coordinates
import Css
import Css.Media
import Html exposing (Html, text)
import Html.Attributes exposing (class, style)
import Html.Events
import Html.Events.Extra.Mouse as Mouse
import Html.Events.Extra.Touch as Touch
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Html.Styled.Lazy
import InfiniteList
import Json.Decode as Decode
import List.Ext as List
import Material.Icons exposing (Coloring(..))
import Material.Icons.Av as Icons
import Material.Icons.Navigation as Icons
import Maybe.Extra as Maybe
import Playlists exposing (Playlist)
import Return3 as Return exposing (..)
import Tachyons
import Tachyons.Classes as T
import Task
import Tracks exposing (..)
import UI.Css
import UI.DnD as DnD
import UI.Kit
import UI.Reply as UI
import UI.Tracks.Reply exposing (..)



-- 🌳


type alias Model =
    { dnd : DnD.Model Int
    , infiniteList : InfiniteList.Model
    }


initialModel : Model
initialModel =
    { dnd = DnD.initialModel
    , infiniteList = InfiniteList.init
    }



-- 📣


type Msg
    = Bypass
    | Reply Reply
      --
    | DragAndDropMsg (DnD.Msg Int)
    | InfiniteListMsg InfiniteList.Model


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        Bypass ->
            return model

        Reply reply ->
            returnReplyWithModel model reply

        --
        InfiniteListMsg infiniteList ->
            return { model | infiniteList = infiniteList }

        DragAndDropMsg subMsg ->
            let
                ( newDnD, uiReplies ) =
                    DnD.update subMsg model.dnd
            in
            if DnD.hasDropped newDnD then
                returnRepliesWithModel
                    { model | dnd = newDnD }
                    [ MoveTrackInSelectedPlaylist
                        { from = Maybe.withDefault 0 <| DnD.modelSubject newDnD
                        , to = Maybe.withDefault 0 <| DnD.modelTarget newDnD
                        }
                    , Transcend uiReplies
                    ]

            else
                returnRepliesWithModel
                    { model | dnd = newDnD }
                    [ Transcend uiReplies ]



-- 🗺


type alias Dependencies =
    { height : Float
    , isVisible : Bool
    }


view : Dependencies -> List IdentifiedTrack -> InfiniteList.Model -> Bool -> Maybe String -> SortBy -> SortDirection -> List Int -> Maybe (DnD.Model Int) -> Html.Styled.Html Msg
view deps harvest infiniteList favouritesOnly searchTerm sortBy sortDirection selectedTrackIndexes maybeDnD =
    brick
        (List.append viewAttributes
            [ Html.Styled.Attributes.tabindex (ifThenElse deps.isVisible 0 -1) ]
        )
        [ C.disable_selection
        , T.flex_grow_1
        , T.outline_0
        , T.overflow_x_hidden
        , T.vh_25

        --
        , case maybeDnD of
            Just dnd ->
                if DnD.isDragging dnd then
                    T.overflow_y_hidden

                else
                    T.overflow_y_auto

            Nothing ->
                T.overflow_y_auto
        ]
        [ -- Header
          ---------
          Html.Styled.Lazy.lazy3
            header
            (Maybe.isJust maybeDnD)
            sortBy
            sortDirection

        -- List
        -------
        , Html.Styled.Lazy.lazy7
            infiniteListView
            deps
            harvest
            infiniteList
            favouritesOnly
            searchTerm
            selectedTrackIndexes
            maybeDnD
        ]


containerId : String
containerId =
    "diffuse__track-list"


infiniteListView : Dependencies -> List IdentifiedTrack -> InfiniteList.Model -> Bool -> Maybe String -> List Int -> Maybe (DnD.Model Int) -> Html.Styled.Html Msg
infiniteListView deps harvest infiniteList favouritesOnly searchTerm selectedTrackIndexes maybeDnD =
    Html.Styled.fromUnstyled
        (InfiniteList.view
            (InfiniteList.withCustomContainer
                infiniteListContainer
                (InfiniteList.config
                    { itemView =
                        case maybeDnD of
                            Just dnd ->
                                playlistItemView
                                    favouritesOnly
                                    searchTerm
                                    selectedTrackIndexes
                                    dnd

                            _ ->
                                defaultItemView
                                    favouritesOnly
                                    selectedTrackIndexes

                    --
                    , itemHeight = InfiniteList.withVariableHeight dynamicRowHeight
                    , containerHeight = round deps.height
                    }
                )
            )
            infiniteList
            harvest
        )


scrollToNowPlaying : List IdentifiedTrack -> IdentifiedTrack -> Cmd Msg
scrollToNowPlaying harvest ( identifiers, _ ) =
    harvest
        |> List.take identifiers.indexInList
        |> List.foldl (\a -> (+) <| dynamicRowHeight 0 a) 0
        |> (\n -> 22 - toFloat rowHeight / 2 + 5 + toFloat n)
        |> Dom.setViewportOf containerId 0
        |> Task.attempt (always Bypass)


scrollToTop : Cmd Msg
scrollToTop =
    Task.attempt (always Bypass) (Dom.setViewportOf containerId 0 0)


viewAttributes =
    [ Html.Styled.Attributes.css viewStyles
    , Html.Styled.Attributes.fromUnstyled (InfiniteList.onScroll InfiniteListMsg)
    , Html.Styled.Attributes.id containerId
    , Html.Styled.Attributes.style "-webkit-overflow-scrolling" "touch"
    , Html.Styled.Attributes.style "overscroll-behavior" "none"
    ]


viewStyles : List Css.Style
viewStyles =
    [ Css.fontSize (Css.px 11.5)
    , Css.Media.withMedia
        [ UI.Css.notSmallMediaQuery ]
        [ Css.fontSize (Css.px 12.5) ]
    ]



-- HEADERS


header : Bool -> SortBy -> SortDirection -> Html.Styled.Html Msg
header isPlaylist sortBy sortDirection =
    let
        sortIcon =
            (if sortDirection == Desc then
                Icons.expand_less

             else
                Icons.expand_more
            )
                15
                Inherit

        sortIconHtml =
            Html.Styled.fromUnstyled sortIcon

        maybeSortIcon s =
            ifThenElse (sortBy == s) (Just sortIconHtml) Nothing
    in
    brick
        [ Html.Styled.Attributes.css headerStyles ]
        [ T.bg_white, T.flex, T.fw6, T.relative, T.z_5 ]
        (if isPlaylist then
            [ headerColumn "" 4.5 First Nothing Bypass
            , headerColumn "#" 4.5 Between Nothing Bypass
            , headerColumn "Title" 36.0 Between Nothing Bypass
            , headerColumn "Artist" 27.5 Between Nothing Bypass
            , headerColumn "Album" 27.5 Last Nothing Bypass
            ]

         else
            [ headerColumn "" 4.5 First Nothing Bypass
            , headerColumn "Title" 37.5 Between (maybeSortIcon Title) (Reply <| SortBy Title)
            , headerColumn "Artist" 29.0 Between (maybeSortIcon Artist) (Reply <| SortBy Artist)
            , headerColumn "Album" 29.0 Last (maybeSortIcon Album) (Reply <| SortBy Album)
            ]
        )


headerStyles : List Css.Style
headerStyles =
    [ Css.borderBottom3 (Css.px 1) Css.solid (Color.toElmCssColor UI.Kit.colors.subtleBorder)
    , Css.color (Color.toElmCssColor headerTextColor)
    , Css.fontSize (Css.px 11)
    ]


headerTextColor : Color.Color
headerTextColor =
    Color.rgb255 207 207 207



-- HEADER COLUMN


type Pos
    = First
    | Between
    | Last


headerColumn :
    String
    -> Float
    -> Pos
    -> Maybe (Html.Styled.Html msg)
    -> msg
    -> Html.Styled.Html msg
headerColumn text_ width pos maybeSortIcon msg =
    brick
        [ Html.Styled.Events.onClick msg
        , Html.Styled.Attributes.css
            [ Css.borderLeft3
                (Css.px <| ifThenElse (pos /= First) 1 0)
                Css.solid
                (Color.toElmCssColor UI.Kit.colors.subtleBorder)
            , Css.property "min-width" columnMinWidth
            , Css.width (Css.pct width)
            ]
        ]
        [ T.lh_title
        , T.pv1
        , T.relative

        --
        , ifThenElse (pos == First) T.pl3 T.pl2
        , ifThenElse (pos == Last) T.pr3 T.pr2
        , ifThenElse (pos == First) "" T.pointer
        ]
        [ brick
            [ Html.Styled.Attributes.css [ Css.top (Css.px 1) ] ]
            [ T.relative ]
            [ Html.Styled.text text_ ]
        , case maybeSortIcon of
            Just sortIcon ->
                brick
                    [ Html.Styled.Attributes.css sortIconStyles ]
                    [ T.absolute, T.mr1, T.right_0 ]
                    [ sortIcon ]

            Nothing ->
                nothing
        ]


sortIconStyles : List Css.Style
sortIconStyles =
    [ Css.fontSize (Css.px 0)
    , Css.lineHeight (Css.px 0)
    , Css.top (Css.pct 50)
    , Css.transform (Css.translateY <| Css.pct -50)
    ]



-- INFINITE LIST


infiniteListContainer :
    List ( String, String )
    -> List (Html msg)
    -> Html msg
infiniteListContainer styles =
    styles
        |> List.map (\( k, v ) -> style k v)
        |> List.append listStyles
        |> Html.div


listStyles : List (Html.Attribute msg)
listStyles =
    [ Tachyons.classes [ T.pb1, T.pt1 ] ]


dynamicRowHeight : Int -> IdentifiedTrack -> Int
dynamicRowHeight _ ( i, t ) =
    let
        shouldRenderGroup =
            i.group
                |> Maybe.map (.firstInGroup >> (==) True)
                |> Maybe.withDefault False
    in
    if shouldRenderGroup then
        32 + 18 + 16 + rowHeight

    else
        rowHeight



-- INFINITE LIST ITEM


defaultItemView : Bool -> List Int -> Int -> Int -> IdentifiedTrack -> Html Msg
defaultItemView favouritesOnly selectedTrackIndexes _ idx identifiedTrack =
    let
        ( identifiers, track ) =
            identifiedTrack

        shouldRenderGroup =
            identifiers.group
                |> Maybe.map (.firstInGroup >> (==) True)
                |> Maybe.withDefault False

        isSelected =
            List.member idx selectedTrackIndexes
    in
    Html.div
        []
        [ if shouldRenderGroup then
            groupNode idx identifiers

          else
            text ""

        --
        , Html.div
            (List.concat
                [ rowStyles idx isSelected identifiers

                --
                , List.append
                    (if isSelected then
                        [ touchContextMenuEvent identifiedTrack Nothing ]

                     else
                        []
                    )
                    [ mouseContextMenuEvent identifiedTrack
                    , playEvent identifiedTrack
                    , selectEvent identifiedTrack
                    ]

                --
                , [ T.flex
                  , T.items_center

                  --
                  , ifThenElse identifiers.isMissing "" T.pointer
                  , ifThenElse isSelected T.fw6 ""
                  ]
                    |> Tachyons.classes
                    |> List.singleton
                ]
            )
            [ favouriteColumn favouritesOnly isSelected identifiers
            , otherColumn "37.5%" False track.tags.title
            , otherColumn "29.0%" False track.tags.artist
            , otherColumn "29.0%" True track.tags.album
            ]
        ]


playlistItemView : Bool -> Maybe String -> List Int -> DnD.Model Int -> Int -> Int -> IdentifiedTrack -> Html Msg
playlistItemView favouritesOnly searchTerm selectedTrackIndexes dnd _ idx identifiedTrack =
    let
        ( identifiers, track ) =
            identifiedTrack

        listIdx =
            identifiers.indexInList

        dragEnv =
            { model = dnd
            , toMsg = DragAndDropMsg
            }

        isSelected =
            List.member idx selectedTrackIndexes
    in
    Html.div
        (List.concat
            [ rowStyles idx isSelected identifiers

            --
            , List.append
                (if isSelected && not favouritesOnly && Maybe.isNothing searchTerm then
                    [ touchContextMenuEvent identifiedTrack (Just dragEnv)
                    , DnD.listenToStart dragEnv listIdx
                    ]

                 else if isSelected then
                    [ touchContextMenuEvent identifiedTrack (Just dragEnv)
                    ]

                 else
                    []
                )
                [ mouseContextMenuEvent identifiedTrack
                , playEvent identifiedTrack
                , selectEvent identifiedTrack
                ]

            --
            , [ T.flex
              , T.items_center

              --
              , ifThenElse identifiers.isMissing "" T.pointer
              , ifThenElse isSelected T.fw6 ""
              ]
                |> Tachyons.classes
                |> List.singleton

            --
            , DnD.listenToEnterLeave dragEnv listIdx

            --
            , if DnD.isBeingDraggedOver listIdx dnd then
                [ dragIndicator ]

              else
                []
            ]
        )
        [ favouriteColumn favouritesOnly isSelected identifiers
        , playlistIndexColumn (Maybe.withDefault 0 identifiers.indexInPlaylist)
        , otherColumn "36.0%" False track.tags.title
        , otherColumn "27.5%" False track.tags.artist
        , otherColumn "27.5%" True track.tags.album
        ]


mouseContextMenuEvent : IdentifiedTrack -> Html.Attribute Msg
mouseContextMenuEvent ( i, _ ) =
    Html.Events.custom
        "contextmenu"
        (Decode.map
            (\event ->
                { message =
                    if event.keys.shift then
                        Bypass

                    else
                        event.clientPos
                            |> Coordinates.fromTuple
                            |> ShowTrackMenuWithSmallDelay i.indexInList
                            |> Reply
                , stopPropagation = True
                , preventDefault = True
                }
            )
            Mouse.eventDecoder
        )


touchContextMenuEvent : IdentifiedTrack -> Maybe (DnD.Environment Int Msg) -> Html.Attribute Msg
touchContextMenuEvent ( i, _ ) maybeDragEnv =
    Html.Events.custom
        "longtap"
        (Decode.map2
            (\x y ->
                { message =
                    -- Only show menu when not dragging something
                    case Maybe.andThen (.model >> DnD.modelTarget) maybeDragEnv of
                        Just _ ->
                            Bypass

                        Nothing ->
                            { x = x, y = y }
                                |> ShowTrackMenuWithoutDelay i.indexInList
                                |> Reply
                , stopPropagation = False
                , preventDefault = False
                }
            )
            (Decode.field "x" Decode.float)
            (Decode.field "y" Decode.float)
        )


playEvent : IdentifiedTrack -> Html.Attribute Msg
playEvent ( i, t ) =
    Html.Events.custom
        "dbltap"
        (Decode.succeed
            { message =
                if i.isMissing then
                    Bypass

                else
                    ( i, t )
                        |> UI.PlayTrack
                        |> List.singleton
                        |> Transcend
                        |> Reply
            , stopPropagation = True
            , preventDefault = True
            }
        )


selectEvent : IdentifiedTrack -> Html.Attribute Msg
selectEvent ( i, _ ) =
    Html.Events.custom
        "tap"
        (Decode.map2
            (\shiftKey button ->
                { message =
                    case button of
                        0 ->
                            { shiftKey = shiftKey }
                                |> MarkAsSelected i.indexInList
                                |> Reply

                        _ ->
                            Bypass
                , stopPropagation = False
                , preventDefault = False
                }
            )
            (Decode.at [ "originalEvent", "shiftKey" ] Decode.bool)
            (Decode.oneOf
                [ Decode.at [ "originalEvent", "button" ] Decode.int
                , Decode.succeed 0
                ]
            )
        )



-- ROWS


groupNode : Int -> Identifiers -> Html Msg
groupNode idx identifiers =
    let
        groupName =
            identifiers.group
                |> Maybe.map .name
                |> Maybe.withDefault "Unknown"
    in
    Html.div
        ([ T.f7
         , T.fw7
         , T.lh_copy
         , T.pb3
         , T.ph3
         , ifThenElse (0 == idx) T.pt3 T.pt4
         , T.truncate
         ]
            |> Tachyons.classes
            |> List.addTo groupStyles
        )
        [ groupIcon
        , Html.span [ class T.v_mid ] [ text groupName ]
        ]


groupIcon : Html msg
groupIcon =
    Html.span
        [ Tachyons.classes [ T.dib, T.pr2, T.v_mid, C.lh_0 ] ]
        [ Icons.library_music 16 Inherit ]


groupStyles : List (Html.Attribute msg)
groupStyles =
    [ style "color" rowFontColors.grey
    , style "font-family" (String.join ", " UI.Kit.headerFontFamilies)
    , style "font-size" "11px"
    , style "letter-spacing" "0.005em"
    ]


rowHeight : Int
rowHeight =
    35


rowStyles : Int -> Bool -> Identifiers -> List (Html.Attribute msg)
rowStyles idx isSelected { isMissing, isNowPlaying } =
    let
        bgColor =
            if isNowPlaying then
                rowBackgroundColors.selection

            else if modBy 2 idx == 1 then
                rowBackgroundColors.whiteNear

            else
                rowBackgroundColors.white

        color =
            if isSelected then
                rowFontColors.selection

            else if isNowPlaying then
                rowFontColors.white

            else if isMissing then
                rowFontColors.grey

            else
                rowFontColors.default
    in
    [ style "background-color" bgColor
    , style "color" color
    , style "height" (String.fromInt rowHeight ++ "px")
    ]



-- COLUMNS


columnMinWidth =
    "28px"


favouriteColumn : Bool -> Bool -> Identifiers -> Html Msg
favouriteColumn favouritesOnly isSelected identifiers =
    Html.div
        ((++)
            [ Html.Events.onClick (Reply <| ToggleFavourite identifiers.indexInList)
            , Tachyons.classes [ T.flex_shrink_0, T.fw4, T.pl3 ]
            ]
            (favouriteColumnStyles favouritesOnly isSelected identifiers)
        )
        [ if identifiers.isFavourite then
            text "t"

          else
            text "f"
        ]


favouriteColumnStyles : Bool -> Bool -> Identifiers -> List (Html.Attribute msg)
favouriteColumnStyles favouritesOnly isSelected { isFavourite, isNowPlaying } =
    let
        color =
            if isNowPlaying && isFavourite then
                if isSelected then
                    favColors.selection

                else
                    favColors.white

            else if isNowPlaying then
                if isSelected then
                    favColors.selectionFaded

                else
                    favColors.whiteFaded

            else if favouritesOnly || not isFavourite then
                if isSelected then
                    favColors.selectionFadedMore

                else
                    favColors.blackFaded

            else if isSelected then
                favColors.selection

            else
                favColors.red
    in
    [ style "color" color
    , style "font-family" "or-favourites"
    , style "min-width" columnMinWidth
    , style "width" "4.5%"
    ]


playlistIndexColumn : Int -> Html msg
playlistIndexColumn indexInPlaylist =
    [ T.pl2
    , T.pr2
    , C.pointer_events_none
    , T.truncate
    ]
        |> Tachyons.classes
        |> List.addTo (otherColumnStyles "4.5%")
        |> (\attributes ->
                Html.div
                    attributes
                    [ text (String.fromInt <| indexInPlaylist + 1) ]
           )


otherColumn : String -> Bool -> String -> Html msg
otherColumn width isLast text_ =
    [ T.pl2
    , C.pointer_events_none
    , T.truncate

    --
    , ifThenElse isLast T.pr3 T.pr2
    ]
        |> Tachyons.classes
        |> List.addTo (otherColumnStyles width)
        |> (\attributes -> Html.div attributes [ text text_ ])


otherColumnStyles : String -> List (Html.Attribute msg)
otherColumnStyles columnWidth =
    [ style "min-width" columnMinWidth
    , style "width" columnWidth
    ]



-- 🖼


favColors =
    { blackFaded = Color.toCssString (Color.rgba 0 0 0 0.125)
    , red = Color.toCssString UI.Kit.colorKit.base08
    , selection = Color.toCssString UI.Kit.colors.selectionAlt
    , selectionFaded = Color.toCssString (Color.fadeOut 0.6 UI.Kit.colors.selectionAlt)
    , selectionFadedMore = Color.toCssString (Color.fadeOut 0.875 UI.Kit.colors.selectionAlt)
    , white = Color.toCssString (Color.rgb 1 1 1)
    , whiteFaded = Color.toCssString (Color.rgba 1 1 1 0.4)
    }


rowBackgroundColors =
    { selection = Color.toCssString UI.Kit.colors.selection
    , white = favColors.white
    , whiteNear = Color.toCssString (Color.rgb255 252 252 252)
    }


rowFontColors =
    { default = Color.toCssString UI.Kit.colors.text
    , grey = Color.toCssString UI.Kit.colorKit.base04
    , selection = favColors.selection
    , white = favColors.white
    }


dragIndicator : Html.Attribute msg
dragIndicator =
    style "box-shadow" ("0 1px 0 0 " ++ Color.toCssString UI.Kit.colorKit.accent ++ " inset")
