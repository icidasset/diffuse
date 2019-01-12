module Tracks.Collection.Internal exposing
    ( arrange
    , build
    , buildf
    , harvest
    , identify
    , initialize
    )

import Flip exposing (flip)
import Tracks exposing (Parcel, Track)
import Tracks.Collection.Internal.Arrange as Internal
import Tracks.Collection.Internal.Harvest as Internal
import Tracks.Collection.Internal.Identify as Internal



-- 🔱


build : List Track -> Parcel -> Parcel
build tracks =
    initialize tracks >> identify >> arrange >> harvest


buildf : Parcel -> List Track -> Parcel
buildf =
    flip build



-- INITIALIZE


initialize : List Track -> Parcel -> Parcel
initialize tracks ( dependencies, collection ) =
    ( dependencies
    , { collection | untouched = tracks }
    )



-- RE-EXPORT


identify : Parcel -> Parcel
identify =
    Internal.identify


arrange : Parcel -> Parcel
arrange =
    Internal.arrange


harvest : Parcel -> Parcel
harvest =
    Internal.harvest
