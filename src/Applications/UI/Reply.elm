module UI.Reply exposing (Reply(..))

import Alien
import Json.Decode as Json
import Queue
import Sources exposing (Source)
import Tracks exposing (IdentifiedTrack)
import UI.Page exposing (Page)



-- 🌳


type Reply
    = ActiveQueueItemChanged (Maybe Queue.Item)
    | AddSourceToCollection Source
    | Chill
    | FillQueue
    | GoToPage Page
    | PlayTrack IdentifiedTrack
    | ProcessSources
    | RemoveTracksWithSourceId String
    | ResetQueue
    | ShiftQueue
    | SaveEnclosedUserData
    | SaveFavourites
    | SaveSources
    | SaveTracks
