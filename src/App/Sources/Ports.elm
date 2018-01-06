port module Sources.Ports exposing (..)

-- 💡


port requestLocalPath : () -> Cmd msg



-- 🚽


port receiveLocalPath : (Maybe String -> msg) -> Sub msg
