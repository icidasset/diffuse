module Maybe.Ext exposing (preferFirst, preferSecond)

import Maybe.Extra


preferFirst : Maybe a -> Maybe a -> Maybe a
preferFirst =
    Maybe.Extra.or


preferSecond : Maybe a -> Maybe a -> Maybe a
preferSecond =
    Maybe.Extra.orElse
