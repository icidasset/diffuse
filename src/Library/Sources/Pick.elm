module Sources.Pick exposing (isMusicFile, selectMusicFiles)

import Regex



-- 🔱


isMusicFile : String -> Bool
isMusicFile =
    Regex.contains musicFileRegex


selectMusicFiles : List String -> List String
selectMusicFiles =
    List.filter isMusicFile



-----------------------------------------
-- ㊙️
-----------------------------------------


musicFileRegex : Regex.Regex
musicFileRegex =
    "\\.(mp3|mp4|m4a|flac|ogg|wave|webm)$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never
