module UI.Alfred exposing (Model, Msg(..), initialModel, subscriptions, update, view)

import Alfred exposing (Alfred)
import Browser.Dom as Dom
import Chunky exposing (..)
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
import Task
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
                , C.fixed
                , C.items_center
                , C.px_3
                , C.cursor_pointer
                , C.z_50
                ]
                [ -----------------------------------------
                  -- Message
                  -----------------------------------------
                  chunk
                    [ C.italic
                    , C.leading_normal
                    , C.mt_12
                    , C.text_center
                    , C.text_white
                    ]
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
                    [ C.text_sm
                    , C.max_w_md
                    , C.mt_8
                    , C.w_full
                    ]
                    [ slab
                        Html.input
                        [ autofocus True
                        , id searchId
                        , onInput DetermineResults
                        , placeholder "Type to search or create"
                        , type_ "text"
                        ]
                        [ C.border_none
                        , C.bg_white
                        , C.block
                        , C.leading_normal
                        , C.rounded
                        , C.outline_none
                        , C.p_4
                        , C.shadow_md
                        , C.text_2xl
                        , C.tracking_tad_closer
                        , C.w_full
                        ]
                        []
                    ]

                -----------------------------------------
                -- Results
                -----------------------------------------
                , chunk
                    [ C.bg_white
                    , C.rounded
                    , C.text_sm
                    , C.leading_none
                    , C.max_w_md
                    , C.mt_8
                    , C.overflow_hidden
                    , C.shadow_md
                    , C.w_full
                    ]
                    (List.indexedMap
                        (\idx result ->
                            brick
                                [ onTapPreventDefault (RunAction idx) ]
                                [ C.p_4
                                , C.relative
                                , C.truncate

                                --
                                , if idx == instance.focus then
                                    C.text_white

                                  else
                                    C.text_inherit

                                --
                                , if idx == instance.focus then
                                    C.bg_accent

                                  else if modBy 2 idx == 0 then
                                    C.bg_transparent

                                  else
                                    C.bg_gray_100
                                ]
                                [ text result

                                --
                                , if idx == instance.focus then
                                    chunk
                                        [ C.absolute
                                        , C.leading_0
                                        , C.minus_translate_y_half
                                        , C.mr_3
                                        , C.right_0
                                        , C.top_half
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
