module Return2 exposing (..)

-- 🌳


type alias Return model msg =
    ( model, Cmd msg )



-- 🔱


andThen : (model -> Return model msg) -> Return model msg -> Return model msg
andThen update ( model, cmd ) =
    let
        ( newModel, newCmd ) =
            update model
    in
    ( newModel
    , Cmd.batch [ cmd, newCmd ]
    )


return : model -> Return model msg
return model =
    ( model, Cmd.none )


returnWithModel : model -> Cmd msg -> Return model msg
returnWithModel =
    Tuple.pair


returnWithCommand : Cmd msg -> model -> Return model msg
returnWithCommand cmd model =
    ( model, cmd )



-- 🔱  ░░  ALIASES


withModel =
    returnWithModel



-- 🔱  ░░  MODIFICATIONS


addCommand : Cmd msg -> Return model msg -> Return model msg
addCommand cmd ( model, earlierCmd ) =
    ( model
    , Cmd.batch [ earlierCmd, cmd ]
    )


mapCommand =
    Cmd.map >> Tuple.mapSecond


mapModel =
    Tuple.mapFirst
