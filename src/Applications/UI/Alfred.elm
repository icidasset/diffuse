module UI.Alfred exposing (Model, Msg(..), initialModel, subscriptions, update, view)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import Chunky exposing (..)
import Classes as C
import Conditional exposing (ifThenElse)
import Css
import Html.Styled as Html exposing (Html, fromUnstyled, text)
import Html.Styled.Attributes exposing (autofocus, css, id, placeholder, type_)
import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled.Ext exposing (onTap)
import Json.Decode
import Keyboard
import List.Extra as List
import Material.Icons exposing (Coloring(..))
import Material.Icons.Hardware as Icons
import Return3 as Return exposing (..)
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
                [ T.absolute__fill
                , T.flex
                , T.flex_column
                , T.fixed
                , T.items_center
                , T.ph3
                , T.pointer
                , T.z_9999
                ]
                [ -----------------------------------------
                  -- Message
                  -----------------------------------------
                  chunk
                    [ T.i, T.lh_copy, T.mt4, T.pt3, T.tc, T.white ]
                    [ text instance.message ]

                -----------------------------------------
                -- Search
                -----------------------------------------
                , brick
                    [ Html.Styled.Events.custom
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
                        , css shadowStyles
                        , id searchId
                        , onInput DetermineResults
                        , placeholder "Type to search or create"
                        , type_ "text"
                        ]
                        [ T.bn
                        , T.bg_white
                        , T.br2
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
                    [ Html.Styled.Events.custom
                        "tap"
                        (Json.Decode.succeed
                            { message = Bypass
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                    , css shadowStyles
                    ]
                    [ T.bg_white
                    , T.br2
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
                                [ onTap (RunAction idx) ]
                                [ T.pa3
                                , T.relative
                                , T.truncate

                                --
                                , if idx == instance.focus then
                                    T.white

                                  else
                                    T.color_inherit

                                --
                                , if idx == instance.focus then
                                    C.bg_base_0D

                                  else if modBy 2 idx == 0 then
                                    T.bg_transparent

                                  else
                                    T.bg_near_white
                                ]
                                [ text result

                                --
                                , if idx == instance.focus then
                                    brick
                                        [ css activeItemIndicatorStyles ]
                                        [ T.absolute
                                        , C.lh_0
                                        , T.mr3
                                        , T.right_0
                                        ]
                                        [ fromUnstyled
                                            (Icons.keyboard_return 13 Inherit)
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
    [ UI.Kit.onOverlayShadow ]
