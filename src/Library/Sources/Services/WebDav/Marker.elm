module Sources.Services.WebDav.Marker exposing (concat, removeOne, takeOne)

{-| Custom `Marker`.

WebDAV doesn't make a recursive list, so we have to manage that ourselves.
The spec seems to have `Depth: Infinity` which should allow that.
But a lot of clients don't implement it.
This basically re-exports `Sources.Services.IPFS.Marker`.

-}

import Sources.Processing exposing (Marker(..))
import Sources.Services.Ipfs.Marker as IpfsMarker



-- IN


concat =
    IpfsMarker.concat



-- OUT


takeOne =
    IpfsMarker.takeOne


removeOne =
    IpfsMarker.removeOne
