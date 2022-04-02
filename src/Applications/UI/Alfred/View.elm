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
                    , "border"
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

                    -- Dark mode
                    ------------
                    , "dark:bg-darkest-hour"
                    , "dark:border-base00"
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
                                [ case result.icon of
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
                                    [ text result.title ]

                                --
                                , if idx == instance.focus then
                                    chunk
                                        [ "leading-0", "ml-2" ]
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
