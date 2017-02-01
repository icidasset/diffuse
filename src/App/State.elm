module State exposing (..)

import Navigation
import Response exposing (..)
import Types exposing (..)


-- CHILDREN

import BackgroundImage.State as BackgroundImage
import Routing.State as Routing


-- INITIAL


initialModel : ProgramFlags -> Navigation.Location -> Model
initialModel flags location =
    { showLoadingScreen = True
    , ------------------------------------
      -- Children
      ------------------------------------
      backgroundImage = BackgroundImage.initialModel
    , routing = Routing.initialModel location
    }


initialCommands : ProgramFlags -> Navigation.Location -> Cmd Msg
initialCommands _ _ =
    Cmd.batch
        [ Cmd.map BackgroundImageMsg BackgroundImage.initialCommands
        , Cmd.map RoutingMsg Routing.initialCommands
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BackgroundImageMsg sub ->
            BackgroundImage.update sub model.backgroundImage
                |> mapModel (\x -> { model | backgroundImage = x })
                |> mapCmd BackgroundImageMsg

        RoutingMsg sub ->
            Routing.update sub model.routing
                |> mapModel (\x -> { model | routing = x })
                |> mapCmd RoutingMsg
