module Sources.Services.Ipfs exposing (defaults, initialData, makeTrackUrl, makeTree, parseErrorResponse, parsePreparationResponse, parseTreeResponse, postProcessTree, prepare, properties)

{-| IPFS Service.

Resources:

  - <https://ipfs.io/docs/api/>

-}

import Dict
import Http
import Sources exposing (Property, SourceData)
import Sources.Processing exposing (..)
import Sources.Services.Common exposing (cleanPath, noPrep)
import Sources.Services.Ipfs.Marker as Marker
import Sources.Services.Ipfs.Parser as Parser
import Time



-- PROPERTIES
-- 📟


defaults =
    { gateway = "http://localhost:8080"
    , name = "Music from IPFS"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "directoryHash"
      , label = "Directory object hash"
      , placeholder = "QmVLDAhCY3X9P2u"
      , password = False
      }
    , { key = "gateway"
      , label = "Read-only gateway"
      , placeholder = defaults.gateway
      , password = False
      }
    ]


{-| Initial data set.
-}
initialData : SourceData
initialData =
    Dict.fromList
        [ ( "directoryHash", "" )
        , ( "name", defaults.name )
        , ( "gateway", defaults.gateway )
        ]



-- PREPARATION


prepare : String -> SourceData -> Marker -> (Result Http.Error String -> msg) -> Maybe (Cmd msg)
prepare _ _ _ _ =
    Nothing



-- TREE


{-| Create a directory tree.
-}
makeTree : SourceData -> Marker -> Time.Posix -> (Result Http.Error String -> msg) -> Cmd msg
makeTree srcData marker _ resultMsg =
    let
        gateway =
            srcData
                |> Dict.get "gateway"
                |> Maybe.withDefault defaults.gateway
                |> String.foldr
                    (\char acc ->
                        if String.isEmpty acc && char == '/' then
                            acc

                        else
                            String.cons char acc
                    )
                    ""

        hash =
            case marker of
                InProgress _ ->
                    marker
                        |> Marker.takeOne
                        |> Maybe.withDefault "MISSING_HASH"

                _ ->
                    srcData
                        |> Dict.get "directoryHash"
                        |> Maybe.withDefault "MISSING_HASH"

        url =
            gateway ++ "/api/v0/ls?arg=" ++ hash ++ "&encoding=json"
    in
    Http.get
        { url = url
        , expect = Http.expectString resultMsg
        }


{-| Re-export parser functions.
-}
parsePreparationResponse : String -> SourceData -> Marker -> PrepationAnswer Marker
parsePreparationResponse =
    noPrep


parseTreeResponse : String -> Marker -> TreeAnswer Marker
parseTreeResponse =
    Parser.parseTreeResponse


parseErrorResponse : String -> String
parseErrorResponse =
    identity



-- POST


{-| Post process the tree results.

!!! Make sure we only use music files that we can use.

-}
postProcessTree : List String -> List String
postProcessTree =
    identity



-- TRACK URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl _ srcData _ hash =
    let
        gateway =
            srcData
                |> Dict.get "gateway"
                |> Maybe.withDefault defaults.gateway
                |> String.foldr
                    (\char acc ->
                        if String.isEmpty acc && char == '/' then
                            acc

                        else
                            String.cons char acc
                    )
                    ""
    in
    gateway ++ "/ipfs/" ++ hash
