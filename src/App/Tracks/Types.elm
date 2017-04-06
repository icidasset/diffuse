module Tracks.Types exposing (..)


type alias Tags =
    { album : Maybe String
    , artist : Maybe String
    , genre : Maybe String
    , title : Maybe String
    , track : Maybe Int
    , year : Maybe Int
    }


type alias TagUrls =
    { getUrl : String
    , headUrl : String
    }


type alias Track =
    { path : String
    , sourceId : String
    , tags : Tags
    }
