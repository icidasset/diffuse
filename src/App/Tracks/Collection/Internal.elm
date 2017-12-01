module Tracks.Collection.Internal
    exposing
        ( build
        , buildf
        , partial
        , initialize
        , identify
        , arrange
        , harvest
        , expose
        )

import Tracks.Types exposing (Parcel, Track)


-- Internal imports

import Tracks.Collection.Internal.Arrange as Internal
import Tracks.Collection.Internal.Expose as Internal
import Tracks.Collection.Internal.Harvest as Internal
import Tracks.Collection.Internal.Identify as Internal


-- 🍯


build : List Track -> Parcel -> Parcel
build tracks =
    initialize tracks >> identify >> arrange >> harvest >> expose


buildf : Parcel -> List Track -> Parcel
buildf =
    flip build



-- Initialize


initialize : List Track -> Parcel -> Parcel
initialize tracks ( model, collection ) =
    (,) model { collection | untouched = tracks }



-- Re-export


partial : Int
partial =
    Internal.partial


identify : Parcel -> Parcel
identify =
    Internal.identify


arrange : Parcel -> Parcel
arrange =
    Internal.arrange


harvest : Parcel -> Parcel
harvest =
    Internal.harvest


expose : Parcel -> Parcel
expose =
    Internal.expose
