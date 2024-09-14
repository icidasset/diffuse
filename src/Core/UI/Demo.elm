module UI.Demo exposing (tape)

import Base64
import Dict
import Json.Encode as Json
import Sources exposing (Service(..), Source)
import Sources.Encoding as Sources
import Time
import Tracks exposing (Favourite, Track)
import Tracks.Encoding as Tracks



-- ⛩


tape : Time.Posix -> Json.Value
tape currentTime =
    Json.object
        [ ( "favourites", Json.list Tracks.encodeFavourite favourites )
        , ( "sources", Json.list Sources.encode sources )
        , ( "tracks", Json.list Tracks.encodeTrack <| tracks currentTime )
        ]



-- ㊙️


favourites : List Favourite
favourites =
    [ { artist = Just "James Blake"
      , title = "Essential Mix (09-17-2011)"
      }
    ]


sources : List Source
sources =
    let
        key =
            "QUtJQTZPUTNFVk1BWFZDRFFINkI="
                |> Base64.decode
                |> Result.withDefault ""

        secret =
            "Z0hPQkdHRzU1aXc0a0RDbjdjWlRJYTVTUDRZWnpERkRzQnFCYWI4Mg=="
                |> Base64.decode
                |> Result.withDefault ""
    in
    [ { id = "15076402187342"
      , data =
            Dict.fromList
                [ ( "accessKey", key )
                , ( "bucketName", "ongaku-ryoho-demo" )
                , ( "directoryPath", "/" )
                , ( "name", "Demo" )
                , ( "region", "us-east-1" )
                , ( "secretKey", secret )
                ]
      , directoryPlaylists = True
      , enabled = True
      , service = AmazonS3
      }
    ]


tracks : Time.Posix -> List Track
tracks insertedAt =
    [ { id = "MTUwNzY0MDIxODczNDIvL0ZyZWUgbXVzaWMvKFNXTDAxMyktb3JpSmFudXMtV0VCLTIwMTQtRlJFRS8wMS1ib25pdGEubXAz"
      , path = "Free music/(SWL013)-oriJanus-WEB-2014-FREE/01-bonita.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 1

            --
            , album = Just "Soulection White Label: 013"
            , artist = Just "oriJanus"
            , title = "Bonita"

            --
            , genre = Just "Soulection"
            , picture = Nothing
            , year = Nothing
            }
      }
    , { id = "MTUwNzY0MDIxODczNDIvL0ZyZWUgbXVzaWMvKFNXTDAxMyktb3JpSmFudXMtV0VCLTIwMTQtRlJFRS8wMi02Lm1wMw"
      , path = "Free music/(SWL013)-oriJanus-WEB-2014-FREE/02-6.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 2

            --
            , album = Just "Soulection White Label: 013"
            , artist = Just "oriJanus"
            , title = "6"

            --
            , genre = Just "Soulection"
            , picture = Nothing
            , year = Nothing
            }
      }
    , { id = "MTUwNzY0MDIxODczNDIvL0ZyZWUgbXVzaWMvKFNXTDAxMyktb3JpSmFudXMtV0VCLTIwMTQtRlJFRS8wMy1ob3RfcmVtaXhfZnQuX3Rlay5sdW5fJl96aWtvbW8ubXAz"
      , path = "Free music/(SWL013)-oriJanus-WEB-2014-FREE/03-hot_remix_ft._tek.lun_&_zikomo.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 3

            --
            , album = Just "Soulection White Label: 013"
            , artist = Just "oriJanus"
            , title = "Hot Remix ft. Tek.Lun & Zikomo"

            --
            , genre = Just "Soulection"
            , picture = Nothing
            , year = Nothing
            }
      }
    , { id = "MTUwNzY0MDIxODczNDIvL0ZyZWUgbXVzaWMvQ29tX1RydWlzZS1DaGVtaWNhbF9MZWdzLTIwMTItRlJFRS8wMS1jb21fdHJ1aXNlLWNoZW1pY2FsX2xlZ3MubXAz"
      , path = "Free music/Com_Truise-Chemical_Legs-2012-FREE/01-com_truise-chemical_legs.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 9

            --
            , album = Just "Adult Swim Singles Project 2012"
            , artist = Just "Com Truise"
            , title = "Chemical Legs"

            --
            , genre = Nothing
            , picture = Nothing
            , year = Just 2012
            }
      }
    , { id = "MTUwNzY0MDIxODczNDIvL0ZyZWUgbXVzaWMvTWFudWVsZV9BdHplbmlfLV8wNF8tX0xpdHRsZV9TdGFyLm1wMw"
      , path = "Free music/Manuele_Atzeni_-_04_-_Little_Star.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 4

            --
            , album = Just "The Miyazaki Tour EP"
            , artist = Just "Manuele Atzeni"
            , title = "Little Star"

            --
            , genre = Just "Funk"
            , picture = Nothing
            , year = Nothing
            }
      }
    , { id = "MTUwNzY0MDIxODczNDIvL0ZyZWUgbXVzaWMvUGF0cmlja19MZWVfLV8wMl8tX1F1aXR0aW5fVGltZS5tcDM"
      , path = "Free music/Patrick_Lee_-_02_-_Quittin_Time.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 2

            --
            , album = Just "The Last Thing"
            , artist = Just "Patrick Lee"
            , title = "Quittin' Time"

            --
            , genre = Just "Electronic"
            , picture = Nothing
            , year = Nothing
            }
      }
    , { id = "MTUwNzY0MDIxODczNDIvL1JhZGlvL2phbWVzX2JsYWtlLWVzc2VudGlhbF9taXgtc2F0LTA5LTE3LTIwMTEubXAz"
      , path = "Radio/james_blake-essential_mix-sat-09-17-2011.mp3"
      , sourceId = "15076402187342"
      , insertedAt = insertedAt
      , tags =
            { disc = 1
            , nr = 1

            --
            , album = Just "Essential Mix-SAT-09-17"
            , artist = Just "James Blake"
            , title = "Essential Mix (09-17-2011)"

            --
            , genre = Just "Electronic"
            , picture = Nothing
            , year = Nothing
            }
      }
    ]
