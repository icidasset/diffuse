module List.Ext exposing (..)


addInFront : List a -> a -> List a
addInFront =
    flip (::)
