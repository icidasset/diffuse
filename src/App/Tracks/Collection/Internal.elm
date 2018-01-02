module Tracks.Collection.Internal
    exposing
        ( build
        , buildf
        , initialize
        , identify
        , arrange
        , harvest
        )

import Tracks.Types exposing (Parcel, Track)


-- Internal imports

import Tracks.Collection.Internal.Arrange as Internal
import Tracks.Collection.Internal.Harvest as Internal
import Tracks.Collection.Internal.Identify as Internal


-- 🍯


build : List Track -> Parcel -> Parcel
build tracks =
    initialize tracks >> identify >> arrange >> harvest


buildf : Parcel -> List Track -> Parcel
buildf =
    flip build



-- Initialize


initialize : List Track -> Parcel -> Parcel
initialize tracks ( model, collection ) =
    (,) model { collection | untouched = tracks }



-- Re-export


identify : Parcel -> Parcel
identify =
    Internal.identify


arrange : Parcel -> Parcel
arrange =
    Internal.arrange


harvest : Parcel -> Parcel
harvest =
    Internal.harvest
