module Tracks.Collection.Internal exposing
    ( arrange
    , harvest
    , identify
    )

import Tracks exposing (Parcel)
import Tracks.Collection.Internal.Arrange as Internal
import Tracks.Collection.Internal.Harvest as Internal
import Tracks.Collection.Internal.Identify as Internal



-- 🔱


identify : Parcel -> Parcel
identify =
    Internal.identify


arrange : Parcel -> Parcel
arrange =
    Internal.arrange


harvest : Parcel -> Parcel
harvest =
    Internal.harvest
