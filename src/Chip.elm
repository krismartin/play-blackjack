module Chip exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class)


type alias Chip =
    { value : Int
    , color : String
    }


newChip : Int -> String -> Chip
newChip value color =
    { value = value
    , color = color
    }


view : Chip -> Html msg
view chip =
    div
        [ class "chip"
        , attribute "style" ("background-color: " ++ chip.color ++ ";")
        ]
        [ span
            []
            [ text ("$" ++ String.fromInt chip.value) ]
        ]
