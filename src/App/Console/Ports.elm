port module Console.Ports exposing (..)

-- 💡


port requestPause : () -> Cmd msg


port requestPlay : () -> Cmd msg


port requestSeek : Float -> Cmd msg



-- 🚽


port setIsPlaying : (Bool -> msg) -> Sub msg
