module Theme exposing (..)

import Element as El
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type alias Theme =
    { bgColor : El.Color
    , button :
        { color : El.Color
        , fontColor : El.Color
        , hoverFontColor : El.Color
        , radius : Int
        , paddingEach : { top : Int, right : Int, bottom : Int, left : Int }
        }
    , fontColor : El.Color
    , fontFamily : List Font.Font
    }


theme : Theme
theme =
    { bgColor = El.rgb255 0 128 0
    , button =
        { color = El.rgba255 1 1 1 0.3
        , fontColor = El.rgba255 177 199 37 0.9
        , hoverFontColor = El.rgba255 255 255 255 0.85
        , radius = 5
        , paddingEach = { top = 15, right = 25, bottom = 15, left = 25 }
        }
    , fontColor = El.rgb255 255 255 255
    , fontFamily =
        [ Font.typeface "Helvetica"
        , Font.serif
        ]
    }


button : List (El.Attribute msg)
button =
    [ Background.color theme.button.color
    , El.paddingEach theme.button.paddingEach
    , Font.color theme.button.fontColor
    , Border.rounded theme.button.radius
    , El.mouseOver [ Font.color theme.button.hoverFontColor ]
    ]
