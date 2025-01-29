module Themes.Sunrise.Alfred.View exposing (view)

import Alfred exposing (..)
import Chunky exposing (..)
import Color exposing (Color)
import Conditional exposing (ifThenElse)
import Html exposing (Html, text)
import Html.Attributes exposing (attribute, autofocus, id, placeholder, style, type_)
import Html.Events exposing (onInput)
import Html.Ext exposing (onTapStopPropagation)
import Json.Decode
import Material.Icons.Round as Icons
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import UI.Types as UI



-- 🗺


view : Maybe (Alfred UI.Msg) -> Maybe Color -> Html UI.Msg
view maybeInstance extractedBackdropColor =
    let
        bgColor =
            Maybe.unwrap "inherit" Color.toCssString extractedBackdropColor
    in
    case maybeInstance of
        Just instance ->
            let
                hasResults =
                    List.sum (List.map (.items >> List.length) instance.results) > 0
            in
            chunk
                [ "inset-0"
                , "flex"
                , "flex-col"
                , "fixed"
                , "items-center"
                , "px-3"
                , "cursor-pointer"
                , "z-50"
                ]
                [ -----------------------------------------
                  -- Message
                  -----------------------------------------
                  chunk
                    [ "italic"
                    , "leading-normal"
                    , "mt-12"
                    , "opacity-75"
                    , "text-center"
                    , "text-white"

                    -- Dark mode
                    ------------
                    , "dark:text-base07"
                    ]
                    [ text instance.message ]

                -----------------------------------------
                -- Search
                -----------------------------------------
                , brick
                    [ Html.Events.custom
                        "tap"
                        (Json.Decode.succeed
                            { message = UI.Bypass
                            , stopPropagation = True
                            , preventDefault = True
                            }
                        )
                    ]
                    [ "text-sm"
                    , "max-w-xl"
                    , "mt-8"
                    , "w-full"
                    ]
                    [ slab
                        Html.input
                        [ autofocus True
                        , id "diffuse__alfred"
                        , onInput UI.GotAlfredInput
                        , type_ "text"

                        --
                        , attribute "spellcheck" "false"

                        --
                        , case instance.operation of
                            Query ->
                                placeholder "Type to search"

                            QueryOrMutation ->
                                placeholder "Type to search or create"

                            Mutation ->
                                placeholder "Type to create"
                        ]
                        [ "border"
                        , "bg-white"
                        , "block"
                        , "leading-normal"
                        , "opacity-95"
                        , "outline-none"
                        , "p-4"
                        , "rounded-t"
                        , "shadow-md"
                        , "text-xl"
                        , "tracking-tad-closer"
                        , "w-full"

                        --
                        , if not hasResults then
                            "rounded-b"

                          else
                            ""

                        -- Dark mode
                        ------------
                        , "dark:bg-darkest-hour"
                        , "dark:border-base00"
                        ]
                        []
                    ]

                -----------------------------------------
                -- Results
                -----------------------------------------
                , brick
                    [ id "alfred__results" ]
                    [ "bg-white"
                    , "border-t-0"
                    , "leading-tight"
                    , "max-w-xl"
                    , "mb-32"
                    , "opacity-95"
                    , "overflow-x-hidden"
                    , "overflow-y-auto"
                    , "rounded-b"
                    , "shadow-md"
                    , "smooth-scrolling"
                    , "text-nearly-sm"
                    , "w-full"

                    --
                    , ifThenElse hasResults "border" ""

                    -- Dark mode
                    ------------
                    , "dark:bg-darkest-hour"
                    , "dark:border-base00"
                    ]
                    (instance.results
                        |> List.foldl
                            (\group ( acc, indexBase ) ->
                                case List.length group.items of
                                    0 ->
                                        ( acc, indexBase )

                                    x ->
                                        ( groupView bgColor instance group indexBase :: acc
                                        , indexBase + x
                                        )
                            )
                            ( [], 0 )
                        |> Tuple.first
                        |> List.reverse
                    )
                ]

        Nothing ->
            nothing


groupView bgColor instance group indexBase =
    raw
        [ case group.name of
            Just name ->
                chunk
                    [ "all-small-caps"
                    , "antialiased"
                    , "font-semibold"
                    , "leading-tight"
                    , "mb-2"
                    , "mx-2"
                    , "mt-5"
                    , "opacity-60"
                    , "px-3"
                    , "text-sm"
                    , "tracking-wider"
                    ]
                    [ Html.text name ]

            Nothing ->
                Html.text ""
        , raw
            (List.indexedMap
                (\i -> itemView bgColor instance <| indexBase + i)
                group.items
            )
        ]


itemView bgColor instance idx item =
    brick
        [ onTapStopPropagation (UI.SelectAlfredItem idx)

        --
        , if idx == instance.focus then
            id "alfred__results__focus"

          else
            id ("alfred__results__" ++ String.fromInt idx)

        --
        , if idx == instance.focus then
            style "background-color" bgColor

          else
            style "" ""
        ]
        (List.concat
            [ [ "flex"
              , "items-center"
              , "m-2"
              , "p-3"
              , "relative"
              , "rounded"
              ]

            --
            , if idx == instance.focus then
                [ "text-white"
                , "dark:opacity-80"
                , "dark:text-base07"
                ]

              else
                [ "text-inherit" ]

            --
            -- , if modBy 2 idx == 0 then
            --     []
            --   else
            --     [ "bg-gray-100", "dark:bg-base01-15" ]
            ]
        )
        [ case item.icon of
            Just icon ->
                slab
                    Html.span
                    []
                    [ "inline-block"
                    , "mr-2"
                    , "w-5"
                    ]
                    [ icon Inherit
                    ]

            Nothing ->
                text ""

        --
        , slab
            Html.span
            []
            [ "flex-1", "inline-block", "pt-px" ]
            [ text item.title ]

        --
        , if idx == instance.focus then
            chunk
                [ "leading-0", "ml-2" ]
                [ Icons.keyboard_return 13 Inherit
                ]

          else
            nothing
        ]
