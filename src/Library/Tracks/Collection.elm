module Tracks.Collection exposing (add, arrange, harvest, harvestChanged, identify, map, tracksChanged)

import Tracks exposing (IdentifiedTrack, Parcel, Track, emptyCollection)
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



-- ⚗️


tracksChanged : List Track -> List Track -> Bool
tracksChanged listA listB =
    case ( listA, listB ) of
        ( [], [] ) ->
            False

        ( a :: restA, b :: restB ) ->
            if a.id /= b.id then
                True

            else
                tracksChanged restA restB

        _ ->
            True


harvestChanged : List IdentifiedTrack -> List IdentifiedTrack -> Bool
harvestChanged listA listB =
    case ( listA, listB ) of
        ( [], [] ) ->
            False

        ( ( _, a ) :: restA, ( _, b ) :: restB ) ->
            if a.id /= b.id then
                True

            else
                harvestChanged restA restB

        _ ->
            True
