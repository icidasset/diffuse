module UI.Alfred exposing (Model, Msg(..), initialModel, subscriptions, update, view)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import Chunky exposing (..)
import Css
import Css.Classes as C
import Html exposing (Html, text)
import Html.Attributes exposing (autofocus, id, placeholder, type_)
import Html.Events exposing (onInput)
import Html.Ext exposing (onTapPreventDefault)
import Json.Decode
import Keyboard
import List.Extra as List
import Material.Icons exposing (Coloring(..))
import Material.Icons.Hardware as Icons
import Return3 exposing (..)
import Tachyons.Classes as T
import Task
import UI.Kit
import UI.Reply exposing (Reply)



-- 🌳


type alias Model =
    { instance : Maybe (Alfred Reply) }


initialModel : Model
initialModel =
    { instance = Nothing }



-- 📣


type Msg
    = Assign (Alfred Reply)
    | Bypass
    | DetermineResults String
    | Hide
    | RunAction Int
      -----------------------------------------
      -- Keyboard
      -----------------------------------------
    | KeyDown (Maybe Keyboard.Key)


update : Msg -> Model -> Return Model Msg Reply
update msg model =
    case msg of
        Assign instance ->
            returnCommandWithModel
                { model | instance = Just instance }
                (Task.attempt (always Bypass) (Dom.focus searchId))

        Bypass ->
            return model

        DetermineResults searchTerm ->
            model.instance
                |> Maybe.map (determineResults searchTerm)
                |> (\a -> return { model | instance = a })

        Hide ->
            return { model | instance = Nothing }

        RunAction index ->
            case model.instance of
                Just instance ->
                    { result = List.getAt index instance.results
                    , searchTerm = instance.searchTerm
                    }
                        |> instance.action
                        |> returnRepliesWithModel model
                        |> andThen (update Hide)

                Nothing ->
                    update Hide model

        -----------------------------------------
        -- Keyboard
        -----------------------------------------
        KeyDown (Just Keyboard.ArrowDown) ->
            case model.instance of
                Just instance ->
                    instance
                        |> (\i -> { i | focus = min (i.focus + 1) (List.length i.results - 1) })
                        |> (\i -> { model | instance = Just i })
                        |> return

                Nothing ->
                    return model

        KeyDown (Just Keyboard.ArrowUp) ->
            case model.instance of
                Just instance ->
                    instance
                        |> (\i -> { i | focus = max (i.focus - 1) 0 })
                        |> (\i -> { model | instance = Just i })
                        |> return

                Nothing ->
                    return model

        KeyDown (Just Keyboard.Enter) ->
            case model.instance of
                Just instance ->
                    update (RunAction instance.focus) model

                Nothing ->
                    return model

        KeyDown _ ->
            return model


determineResults : String -> Alfred Reply -> Alfred Reply
determineResults searchTerm alfred =
    let
        lowerSearchTerm =
            searchTerm
                |> String.toLower
                |> String.trim
    in
    if String.length lowerSearchTerm > 0 then
        { alfred
            | searchTerm =
                Just searchTerm
            , results =
                alfred.index
                    |> List.filter (String.toLower >> String.contains lowerSearchTerm)
                    |> List.sort
        }

    else
        { alfred
            | searchTerm = Nothing
            , results = alfred.index
        }



-- 📰


subscriptions : Model -> Sub Msg
subscriptions _ =
    Keyboard.downs (Keyboard.anyKeyUpper >> KeyDown)



-- 🗺


view : Model -> Html Msg
view model =
    case model.instance of
        Just instance ->
            chunk
                [ C.inset_0
                , C.flex
                , C.flex_col
                , T.fixed
                , C.items_center
                , C.px_3
                , C.cursor_pointer
                , T.z_9999
                ]
                [ -----------------------------------------
                  -- Message
                  -----------------------------------------
                  chunk
                    [ T.i, T.lh_copy, T.mt4, C.pt_3, C.text_center, T.white ]
                    [ text instance.message ]

                -----------------------------------------
                -- Search
                -----------------------------------------
                , brick
                    [ Html.Events.custom
                        "tap"
                        (Json.Decode.succeed
                            { message = Bypass
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                    ]
                    [ T.f6
                    , T.measure_wide
                    , T.mt4
                    , T.w_100
                    ]
                    [ slab
                        Html.input
                        [ autofocus True

                        -- TODO: , css shadowStyles
                        , id searchId
                        , onInput DetermineResults
                        , placeholder "Type to search or create"
                        , type_ "text"
                        ]
                        [ T.bn
                        , C.bg_white
                        , C.rounded
                        , T.db
                        , T.f3
                        , T.lh_copy
                        , T.outline_0
                        , T.pa3
                        , T.w_100
                        ]
                        []
                    ]

                -----------------------------------------
                -- Results
                -----------------------------------------
                , brick
                    -- TODO: [ css shadowStyles ]
                    []
                    [ C.bg_white
                    , C.rounded
                    , T.f6
                    , T.lh_solid
                    , T.measure_wide
                    , T.mid_gray
                    , T.mt4
                    , T.overflow_hidden
                    , T.w_100
                    ]
                    (List.indexedMap
                        (\idx result ->
                            brick
                                [ onTapPreventDefault (RunAction idx) ]
                                [ T.pa3
                                , C.relative
                                , T.truncate

                                --
                                , if idx == instance.focus then
                                    T.white

                                  else
                                    T.color_inherit

                                --
                                , if idx == instance.focus then
                                    C.bg_base0d

                                  else if modBy 2 idx == 0 then
                                    T.bg_transparent

                                  else
                                    T.bg_near_white
                                ]
                                [ text result

                                --
                                , if idx == instance.focus then
                                    brick
                                        -- TODO: [ css activeItemIndicatorStyles ]
                                        []
                                        [ C.absolute
                                        , C.leading_0
                                        , T.mr3
                                        , T.right_0
                                        ]
                                        [ Icons.keyboard_return 13 Inherit
                                        ]

                                  else
                                    nothing
                                ]
                        )
                        instance.results
                    )
                ]

        Nothing ->
            nothing


searchId =
    "diffuse__alfred"



-- 🖼


activeItemIndicatorStyles : List Css.Style
activeItemIndicatorStyles =
    [ Css.top (Css.pct 50)
    , Css.transform (Css.translateY <| Css.pct -50)
    ]


shadowStyles : List Css.Style
shadowStyles =
    -- TODO: [ UI.Kit.onOverlayShadow ]
    []
