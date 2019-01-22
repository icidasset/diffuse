module Tracks.Collection exposing (add, arrange, harvest, identify, map, removeByPaths, removeBySourceId)

import Flip exposing (flip)
import List.Extra as List
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


removeByPaths : String -> List String -> Parcel -> Parcel
removeByPaths sourceId paths ( deps, { untouched } ) =
    let
        ( filtered, _ ) =
            List.foldr
                (\t ( acc, remainingPaths ) ->
                    if t.sourceId == sourceId && List.member t.path remainingPaths then
                        ( acc, List.remove t.path remainingPaths )

                    else
                        ( t :: acc, remainingPaths )
                )
                ( [], paths )
                untouched
    in
    identify
        ( deps
        , { emptyCollection | untouched = filtered }
        )


removeBySourceId : String -> Parcel -> Parcel
removeBySourceId sourceId ( deps, { untouched } ) =
    identify
        ( deps
        , { emptyCollection | untouched = List.filter (.sourceId >> (/=) sourceId) untouched }
        )
