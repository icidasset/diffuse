module Sources.Services.Ipfs exposing (..)

{-| IPFS Service.

Resources:

  - <https://ipfs.io/docs/api/>

-}

import Date exposing (Date)
import Dict
import Http
import Sources.Services.Ipfs.Marker as Marker
import Sources.Services.Ipfs.Parser as Parser
import Sources.Types exposing (..)


-- Properties
-- 📟


defaults =
    { gateway = "http://localhost:8080"
    , name = "IPFS source"
    }


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List ( String, String, String, Bool )
properties =
    [ ( "directoryHash", "Directory object hash", "QmVLDAhCY3X9P2u", False )
    , ( "gateway", "Read-only gateway", defaults.gateway, False )
    , ( "name", "Label", defaults.name, False )
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



-- Track URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Date -> SourceData -> HttpMethod -> String -> String
makeTrackUrl currentDate srcData _ hash =
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



-- Tree


{-| Create a directory tree.

List all the tracks in the bucket.
Or a specific directory in the bucket.

-}
makeTree : SourceData -> Marker -> (TreeStepResult -> msg) -> Date -> Cmd msg
makeTree srcData marker msg currentDate =
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
                InProgress progress ->
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
        url
            |> Http.getString
            |> Http.send msg


{-| Re-export parser functions.
-}
parseTreeResponse : String -> Marker -> ParsedResponse Marker
parseTreeResponse =
    Parser.parseTreeResponse


parseErrorResponse : String -> String
parseErrorResponse =
    identity



-- Post


{-| Post process the tree results.

!!! Make sure we only use music files that we can use.

-}
postProcessTree : List String -> List String
postProcessTree =
    identity
