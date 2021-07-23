module UI.Alfred.View exposing (view)

import Alfred exposing (..)
import Chunky exposing (..)
import Color exposing (Color)
import Html exposing (Html, text)
import Html.Attributes exposing (autofocus, id, placeholder, style, type_)
import Html.Events exposing (onInput)
import Html.Ext exposing (onTapPreventDefault)
import Json.Decode
import Material.Icons as Icons
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
                    , "max-w-lg"
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
                        , case instance.operation of
                            Query ->
                                placeholder "Type to search"

                            QueryOrMutation ->
                                placeholder "Type to search or create"

                            Mutation ->
                                placeholder "Type to create"
                        ]
                        [ "border-none"
                        , "bg-white"
                        , "block"
                        , "leading-normal"
                        , "rounded"
                        , "outline-none"
                        , "p-4"
                        , "shadow-md"
                        , "text-2xl"
                        , "tracking-tad-closer"
                        , "w-full"

                        -- Dark mode
                        ------------
                        , "dark:bg-base00"
                        ]
                        []
                    ]

                -----------------------------------------
                -- Results
                -----------------------------------------
                , brick
                    [ id "alfred__results" ]
                    [ "bg-white"
                    , "rounded"
                    , "leading-none"
                    , "max-w-lg"
                    , "mb-32"
                    , "mt-8"
                    , "overflow-x-hidden"
                    , "overflow-y-auto"
                    , "shadow-md"
                    , "smooth-scrolling"
                    , "text-nearly-sm"
                    , "w-full"

                    -- Dark mode
                    ------------
                    , "dark:bg-base00"
                    ]
                    (List.indexedMap
                        (\idx result ->
                            brick
                                [ onTapPreventDefault (UI.SelectAlfredItem idx)

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
                                    [ [ "p-4"
                                      , "relative"
                                      , "truncate"
                                      ]

                                    --
                                    , if idx == instance.focus then
                                        [ "text-white", "dark:text-base07" ]

                                      else
                                        [ "text-inherit" ]

                                    --
                                    , if modBy 2 idx == 0 then
                                        []

                                      else
                                        [ "bg-gray-100", "dark:bg-base01-15" ]
                                    ]
                                )
                                [ text result

                                --
                                , if idx == instance.focus then
                                    chunk
                                        [ "absolute"
                                        , "leading-0"
                                        , "-translate-y-1/2"
                                        , "mr-3"
                                        , "right-0"
                                        , "top-1/2"
                                        , "transform"
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
