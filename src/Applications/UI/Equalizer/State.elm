module UI.Equalizer.State exposing (..)

import Chunky exposing (..)
import Chunky.Styled
import Color exposing (Color)
import Color.Ext as Color
import Common
import Coordinates exposing (Coordinates)
import Css
import Css.Classes as C
import Css.Ext as Css
import Equalizer exposing (..)
import Html exposing (Html)
import Html.Events.Extra.Pointer as Pointer
import Html.Styled
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events
import Json.Decode as Decode
import Management
import Material.Icons as Icons
import Monocle.Lens as Lens exposing (Lens)
import Return exposing (return)
import Svg.Styled
import Svg.Styled.Attributes
import UI.Equalizer.Types as Equalizer exposing (..)
import UI.Kit
import UI.Navigation exposing (..)
import UI.Page
import UI.Ports as Ports
import UI.Reply exposing (Reply(..))
import UI.Types as UI exposing (..)
import UI.User.State.Export as User



-- 🌳


type alias Model =
    { low : Float
    , mid : Float
    , high : Float
    , volume : Float

    --
    , activeKnob : Maybe Knob
    , startCoordinates : Coordinates
    }


initialModel : Model
initialModel =
    { low = defaultSettings.low
    , mid = defaultSettings.mid
    , high = defaultSettings.high
    , volume = defaultSettings.volume

    -- Knob interactions
    --------------------
    , activeKnob = Nothing
    , startCoordinates = { x = 0, y = 0 }
    }


lens : Lens UI.Model Equalizer.Model
lens =
    { get = .equalizer
    , set = \equalizer ui -> { ui | equalizer = equalizer }
    }



-- 📣


organize : Organizer Equalizer.Model -> Manager
organize =
    Management.organize lens



-- 🔱


activateKnob : Knob -> Pointer.Event -> Organizer Equalizer.Model
activateKnob theKnob { pointer } model =
    Return.singleton
        { model
            | activeKnob = Just theKnob
            , startCoordinates = Coordinates.fromTuple pointer.clientPos
        }


adjustKnob : Pointer.Event -> Organizer Equalizer.Model
adjustKnob { pointer } model =
    let
        start =
            model.startCoordinates

        end =
            (\( a, b ) -> { x = a, y = b })
                pointer.clientPos

        x =
            end.x - start.x

        y =
            start.y - end.y

        distance =
            sqrt (x ^ 2 + y ^ 2)

        angle =
            atan2 x y
                * (180 / pi)
                |> max (maxAngle * -1)
                |> min maxAngle

        value =
            case ( distance > 10, model.activeKnob ) of
                ( True, Just Volume ) ->
                    Just ( Volume, (maxAngle + angle) / (maxAngle * 2) )

                ( True, Just knobType ) ->
                    Just ( knobType, angle / maxAngle )

                _ ->
                    Nothing

        newModel =
            case value of
                Just ( Low, v ) ->
                    { model | low = v }

                Just ( Mid, v ) ->
                    { model | mid = v }

                Just ( High, v ) ->
                    { model | high = v }

                Just ( Volume, v ) ->
                    { model | volume = v }

                Nothing ->
                    model
    in
    case value of
        Just ( knobType, v ) ->
            return newModel (adjustKnobUsingPort knobType v)

        Nothing ->
            Return.singleton newModel


deactivateKnob : Manager
deactivateKnob model =
    model
        |> organize (\m -> Return.singleton { m | activeKnob = Nothing })
        |> Return.andThen User.saveEnclosedUserData


resetKnob : Knob -> Manager
resetKnob knob model =
    model
        |> organize (resetKnobOrganizer knob)
        |> Return.andThen User.saveEnclosedUserData



-- ⚗️


adjustKnobUsingPort : Knob -> Float -> Cmd Msg
adjustKnobUsingPort knobType value =
    Ports.adjustEqualizerSetting
        { value = value
        , knob =
            case knobType of
                Low ->
                    "LOW"

                Mid ->
                    "MID"

                High ->
                    "HIGH"

                Volume ->
                    "VOLUME"
        }


adjustAllKnobs : Equalizer.Model -> Cmd Msg
adjustAllKnobs model =
    Cmd.batch
        [ adjustKnobUsingPort Low model.low
        , adjustKnobUsingPort Mid model.mid
        , adjustKnobUsingPort High model.high
        , adjustKnobUsingPort Volume model.volume
        ]



-- ㊙️


resetKnobOrganizer : Knob -> Organizer Equalizer.Model
resetKnobOrganizer knob model =
    case knob of
        Low ->
            reset { model | low = defaultSettings.low } Low defaultSettings.low

        Mid ->
            reset { model | mid = defaultSettings.mid } Mid defaultSettings.mid

        High ->
            reset { model | high = defaultSettings.high } High defaultSettings.high

        Volume ->
            reset { model | volume = defaultSettings.volume } Volume defaultSettings.volume


reset : Equalizer.Model -> Knob -> Float -> ( Equalizer.Model, Cmd Msg )
reset newModel knobType value =
    ( newModel
    , adjustKnobUsingPort knobType value
    )
