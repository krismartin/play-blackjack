module Chip exposing (..)

import Html exposing (Html, div, span, text)
import Html.Attributes exposing (attribute, class, classList)


type alias Chip =
    { value : Int
    , color : String
    , active : Bool
    }


newChip : Int -> String -> Chip
newChip value color =
    { value = value
    , color = color
    , active = True
    }


view : List (Html.Attribute msg) -> Chip -> Html msg
view attributes chip =
    let
        { active, color, value } =
            chip

        attrs =
            [ classList [ ( "chip", True ), ( "chip--disabled", active == False ) ]
            , attribute "style" ("background-color: " ++ color ++ ";")
            ]
                ++ attributes
    in
    div
        attrs
        [ span
            []
            [ text ("$" ++ String.fromInt value) ]
        ]
