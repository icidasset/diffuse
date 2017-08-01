module Sources.Pick exposing (..)

import Regex


isMusicFile : String -> Bool
isMusicFile =
    Regex.contains (Regex.regex "\\.(mp3|mp4|m4a)$")


selectMusicFiles : List String -> List String
selectMusicFiles =
    List.filter isMusicFile
