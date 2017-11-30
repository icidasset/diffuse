module Tracks.Collection.Internal.Expose exposing (expose, partial)

import Tracks.Types exposing (Parcel)


-- 🍯


partial : Int
partial =
    50


expose : Parcel -> Parcel
expose ( model, collection ) =
    (,)
        model
        { collection | exposed = List.take (model.exposedStep * partial) collection.harvested }
