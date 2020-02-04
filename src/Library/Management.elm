module Management exposing (..)

import Monocle.Lens as Lens exposing (Lens)



-- 📣


type alias Manager msg model =
    model -> ( model, Cmd msg )



-- 🔱


{-| For working with nested models.

    organize : Manager Msg NestedModel -> Manager Msg Model
    organize =
        { get = .nested
        , set = \nested ui -> { ui | nested = nested }
        }
            |> Monocle.Lens.Lens
            |> Management.organize

    update : Nested.Msg -> Manager Msg Model
    update msg =
        case msg of
            NestedMsg ->
                organize handleNestedMsg

-}
organize :
    Lens parent nested
    -> Manager msg nested
    -> Manager msg parent
organize lens manager parent =
    parent
        |> lens.get
        |> manager
        |> Tuple.mapFirst (\nested -> lens.set nested parent)
