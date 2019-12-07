module Sources.Services.Btfs exposing (..)

{-| BTFS Service.

Resources:

  - <https://docs.btfs.io/>

-}

import Dict
import Sources exposing (Property, SourceData)
import Sources.Processing exposing (..)
import Sources.Services.Ipfs as Ipfs
import Time



-- PROPERTIES
-- 📟


defaults =
    { gateway = ""
    , name = "Music from BTFS"
    }


defaultGateway =
    "http://127.0.0.1:8080"


{-| The list of properties we need from the user.

Tuple: (property, label, placeholder, isPassword)
Will be used for the forms.

-}
properties : List Property
properties =
    [ { key = "directoryHash"
      , label = "Directory object hash / DNSLink domain"
      , placeholder = "QmVLDAhCY3X9P2u"
      , password = False
      }
    , { key = "gateway"
      , label = "Gateway (Optional)"
      , placeholder = defaultGateway
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


prepare =
    Ipfs.prepare



-- TREE


{-| Create a directory tree.
-}
makeTree =
    Ipfs.makeTree


{-| Re-export parser functions.
-}
parsePreparationResponse =
    Ipfs.parsePreparationResponse


parseTreeResponse =
    Ipfs.parseTreeResponse


parseErrorResponse =
    Ipfs.parseErrorResponse



-- POST


{-| Post process the tree results.

!!! Make sure we only use music files that we can use.

-}
postProcessTree =
    Ipfs.postProcessTree



-- TRACK URL


{-| Create a public url for a file.

We need this to play the track.

-}
makeTrackUrl : Time.Posix -> SourceData -> HttpMethod -> String -> String
makeTrackUrl _ srcData _ path =
    Ipfs.extractGateway srcData ++ "/btfs/" ++ Ipfs.rootHash srcData ++ "/" ++ Ipfs.encodedPath path
