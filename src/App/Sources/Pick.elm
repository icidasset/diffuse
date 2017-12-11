module Sources.Pick exposing (isMusicFile, selectMusicFiles)

import Regex


isMusicFile : String -> Bool
isMusicFile =
    Regex.contains musicFileRegex


selectMusicFiles : List String -> List String
selectMusicFiles =
    List.filter isMusicFile



-- Private


musicFileRegex : Regex.Regex
musicFileRegex =
    "\\.(mp3|mp4|m4a|flac)$"
        |> Regex.regex
        |> Regex.caseInsensitive
