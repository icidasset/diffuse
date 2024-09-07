module UI.Tracks.Covers exposing (..)

import Base64
import Conditional exposing (ifThenElse)
import Dict
import Maybe.Extra as Maybe
import Tracks exposing (..)



-- 🔱


generate :
    SortBy
    -> Tracks.Collection
    -> CoverCollection
generate sortBy tracks =
    let
        groupFn =
            coverGroup sortBy

        makeCoverFn =
            makeCover sortBy
    in
    tracks.arranged
        |> List.foldr
            (\identifiedTrack { covers, gathering } ->
                let
                    group =
                        groupFn identifiedTrack

                    ( identifiers, track ) =
                        identifiedTrack

                    { artist, album } =
                        track.tags
                in
                if group /= gathering.previousGroup then
                    -- New group, make cover for previous group
                    let
                        collection =
                            makeCoverFn gathering covers
                    in
                    { gathering =
                        { acc = [ identifiedTrack ]
                        , accIds = [ track.id ]
                        , previousGroup = group
                        , previousTrack = track

                        --
                        , currentAlbumSequence = Just ( identifiedTrack, 1 )
                        , largestAlbumSequence = Nothing

                        --
                        , currentAlbumFavsSequence = Just ( identifiedTrack, ifThenElse identifiers.isFavourite 1 0 )
                        , largestAlbumFavsSequence = Nothing

                        --
                        , currentArtistSequence = Just ( identifiedTrack, 1 )
                        , largestArtistSequence = Nothing
                        }
                    , covers =
                        collection
                    }

                else
                    -- Same group
                    { gathering =
                        { acc = identifiedTrack :: gathering.acc
                        , accIds = track.id :: gathering.accIds
                        , previousGroup = group
                        , previousTrack = track

                        -- Album sequence
                        -----------------
                        , currentAlbumSequence =
                            if album /= gathering.previousTrack.tags.album then
                                Just ( identifiedTrack, 1 )

                            else
                                increaseSequence gathering.currentAlbumSequence

                        --
                        , largestAlbumSequence =
                            if album /= gathering.previousTrack.tags.album then
                                resolveLargestSequence
                                    gathering.currentAlbumSequence
                                    gathering.largestAlbumSequence

                            else
                                gathering.largestAlbumSequence

                        -- Album favourites sequence
                        ----------------------------
                        , currentAlbumFavsSequence =
                            if album /= gathering.previousTrack.tags.album then
                                Just ( identifiedTrack, ifThenElse identifiers.isFavourite 1 0 )

                            else if identifiers.isFavourite then
                                increaseSequence gathering.currentAlbumFavsSequence

                            else
                                gathering.currentAlbumFavsSequence

                        --
                        , largestAlbumFavsSequence =
                            if album /= gathering.previousTrack.tags.album then
                                resolveLargestSequence
                                    gathering.currentAlbumFavsSequence
                                    gathering.largestAlbumFavsSequence

                            else
                                gathering.largestAlbumFavsSequence

                        -- Artist sequence
                        ------------------
                        , currentArtistSequence =
                            if artist /= gathering.previousTrack.tags.artist then
                                Just ( identifiedTrack, 1 )

                            else
                                increaseSequence gathering.currentArtistSequence

                        --
                        , largestArtistSequence =
                            if artist /= gathering.previousTrack.tags.artist then
                                resolveLargestSequence
                                    gathering.currentArtistSequence
                                    gathering.largestArtistSequence

                            else
                                gathering.largestArtistSequence
                        }
                    , covers =
                        covers
                    }
            )
            { covers =
                []
            , gathering =
                { acc = []
                , accIds = []
                , previousGroup = ""
                , previousTrack = emptyTrack

                --
                , currentAlbumSequence = Nothing
                , largestAlbumSequence = Nothing
                , currentAlbumFavsSequence = Nothing
                , largestAlbumFavsSequence = Nothing
                , currentArtistSequence = Nothing
                , largestArtistSequence = Nothing
                }
            }
        |> (\{ covers, gathering } ->
                makeCoverFn gathering covers
           )
        |> (\collection ->
                { arranged = collection, harvested = [] }
           )


harvest :
    Maybe Cover
    -> SortBy
    -> Tracks.Collection
    -> CoverCollection
    -> ( CoverCollection, Maybe Cover )
harvest previouslySelectedCover sortBy tracks covers =
    let
        groupFn =
            coverGroup sortBy

        ( groups, tracksPerGroup ) =
            List.foldr
                (\identifiedTrack ( acc, dict ) ->
                    let
                        group =
                            groupFn identifiedTrack
                    in
                    ( if Dict.member group dict == False then
                        group :: acc

                      else
                        acc
                      --
                    , Dict.update group
                        (Maybe.unwrap [ identifiedTrack ] ((::) identifiedTrack) >> Just)
                        dict
                    )
                )
                ( [], Dict.empty )
                tracks.harvested
    in
    covers.arranged
        |> List.foldr
            (\cover ( acc, sel ) ->
                if List.member cover.group groups then
                    let
                        groupTracks =
                            Maybe.withDefault [] (Dict.get cover.group tracksPerGroup)

                        trackIds =
                            List.map (Tuple.second >> .id) groupTracks

                        harvestedCover =
                            { cover | tracks = groupTracks, trackIds = trackIds }
                    in
                    case ( previouslySelectedCover, sel ) of
                        ( Just pre, Nothing ) ->
                            ( harvestedCover :: acc
                            , if pre.key == harvestedCover.key then
                                Just harvestedCover

                              else
                                Nothing
                            )

                        ( Just _, Just s ) ->
                            ( harvestedCover :: acc
                            , Just s
                            )

                        ( Nothing, _ ) ->
                            ( harvestedCover :: acc
                            , Nothing
                            )

                else
                    ( acc
                    , sel
                    )
            )
            ( []
            , Nothing
            )
        |> Tuple.mapFirst
            (\h -> { covers | harvested = h })



-- ⚗️


makeCover sortBy_ gathering collection =
    let
        closedGathering =
            { gathering
                | largestAlbumSequence =
                    resolveLargestSequence
                        gathering.currentAlbumSequence
                        gathering.largestAlbumSequence

                --
                , largestAlbumFavsSequence =
                    resolveLargestSequence
                        gathering.currentAlbumFavsSequence
                        gathering.largestAlbumFavsSequence

                --
                , largestArtistSequence =
                    resolveLargestSequence
                        gathering.currentArtistSequence
                        gathering.largestArtistSequence
            }
    in
    case closedGathering.acc of
        [] ->
            collection

        fallback :: _ ->
            makeCoverWithFallback sortBy_ closedGathering fallback :: collection


makeCoverWithFallback _ gathering fallback =
    let
        amountOfTracks =
            List.length gathering.accIds

        group =
            gathering.previousGroup

        identifiedTrack : IdentifiedTrack
        identifiedTrack =
            gathering.largestAlbumFavsSequence
                |> Maybe.orElse gathering.largestAlbumSequence
                |> Maybe.map Tuple.first
                |> Maybe.withDefault fallback

        ( _, track ) =
            identifiedTrack

        ( largestAlbumSequence, largestArtistSequence ) =
            ( Maybe.unwrap 0 Tuple.second gathering.largestAlbumSequence
            , Maybe.unwrap 0 Tuple.second gathering.largestArtistSequence
            )

        ( sameAlbum, sameArtist ) =
            ( largestAlbumSequence == amountOfTracks
            , largestArtistSequence == amountOfTracks
            )

        isVariousArtists =
            False
                || (amountOfTracks > 4 && largestArtistSequence < 3)
                || (Maybe.map String.toLower track.tags.artist == Just "va")
    in
    { key = Base64.encode (coverKey isVariousArtists track)
    , identifiedTrackCover = identifiedTrack

    --
    , group = group
    , sameAlbum = sameAlbum
    , sameArtist = sameArtist

    --
    , trackIds = gathering.accIds
    , tracks = gathering.acc
    , variousArtists = isVariousArtists
    }



-- ⚗️  ░░  SEQUENCES


increaseSequence =
    Maybe.map (Tuple.mapSecond ((+) 1))


resolveLargestSequence curr state =
    case ( curr, state ) of
        ( Just ( _, c ), Just ( _, s ) ) ->
            ifThenElse (c > s) curr state

        ( Just _, Nothing ) ->
            curr

        ( Nothing, Just _ ) ->
            state

        ( Nothing, Nothing ) ->
            Nothing
