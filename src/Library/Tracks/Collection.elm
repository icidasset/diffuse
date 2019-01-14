module Tracks.Collection exposing (add, arrange, harvest, identify, map)

import Flip exposing (flip)
import Tracks exposing (..)
import Tracks.Collection.Internal as Internal



-- 🔱


identify : Parcel -> Parcel
identify =
    Internal.identify >> Internal.arrange >> Internal.harvest


arrange : Parcel -> Parcel
arrange =
    Internal.arrange >> Internal.harvest


harvest : Parcel -> Parcel
harvest =
    Internal.harvest


map : (List IdentifiedTrack -> List IdentifiedTrack) -> Parcel -> Parcel
map fn ( model, collection ) =
    ( model
    , { collection
        | identified = fn collection.identified
        , arranged = fn collection.arranged
        , harvested = fn collection.harvested
      }
    )



-- ⚗️


add : List Track -> Parcel -> Parcel
add tracks ( deps, { untouched } ) =
    identify
        ( deps
        , { emptyCollection | untouched = untouched ++ tracks }
        )
