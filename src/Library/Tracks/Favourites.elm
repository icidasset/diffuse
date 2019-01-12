module Tracks.Favourites exposing (match)

import Tracks exposing (Favourite, Track)



-- 🔱


match : Favourite -> Favourite -> Bool
match a b =
    let
        ( aa, at ) =
            ( String.toLower a.artist
            , String.toLower a.title
            )

        ( ba, bt ) =
            ( String.toLower b.artist
            , String.toLower b.title
            )
    in
    aa == ba && at == bt
