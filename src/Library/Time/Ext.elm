module Time.Ext exposing (decoder, default, encode, monthName, monthNumber)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Json
import Time exposing (Month(..))



-- 🔱


decoder : Decoder Time.Posix
decoder =
    Decode.map Time.millisToPosix Decode.int


default : Time.Posix
default =
    Time.millisToPosix 0


encode : Time.Posix -> Json.Value
encode time =
    Json.int (Time.posixToMillis time)


monthName : Time.Month -> String
monthName month =
    case month of
        Jan ->
            "January"

        Feb ->
            "February"

        Mar ->
            "March"

        Apr ->
            "April"

        May ->
            "May"

        Jun ->
            "June"

        Jul ->
            "July"

        Aug ->
            "August"

        Sep ->
            "September"

        Oct ->
            "October"

        Nov ->
            "November"

        Dec ->
            "December"


monthNumber : Time.Month -> Int
monthNumber month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12
