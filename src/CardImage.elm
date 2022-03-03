module CardImage exposing (cardImage)

import Cards
import Html exposing (Html, div, label, span, text)
import Html.Attributes exposing (class, classList)


rank : Cards.Suit -> String
rank suit =
    case suit of
        Cards.Spades ->
            "♠"

        Cards.Hearts ->
            "♥"

        Cards.Diamonds ->
            "♦"

        Cards.Clubs ->
            "♣"


cardImage : Cards.Card -> Html msg
cardImage card =
    case card of
        Cards.Card suit face ->
            let
                cardRank =
                    rank suit

                value =
                    Cards.defaultFace face

                ( cardSymbol, cardLabel ) =
                    if face == Cards.Jack then
                        ( "J" ++ cardRank, "⚔" )

                    else if face == Cards.Queen then
                        ( "Q" ++ cardRank, "♛" )

                    else if face == Cards.King then
                        ( "K" ++ cardRank, "♚" )

                    else if face == Cards.Ace then
                        ( "A" ++ cardRank, cardRank )

                    else
                        ( String.fromInt value ++ cardRank, String.repeat value (cardRank ++ " ") )
            in
            div [ classList [ ( "playing-card", True ), ( "playing-card--flip", True ) ] ]
                [ label [ class cardRank ]
                    [ div [ class "card-front", Html.Attributes.attribute "data-card" cardSymbol ]
                        [ span [] [ text cardLabel ] ]
                    ]
                ]

        Cards.Back ->
            div [ class "playing-card" ]
                [ label [ class "" ]
                    [ div [ class "card-front", Html.Attributes.attribute "data-card" "" ]
                        [ span [] [ text "" ] ]
                    ]
                ]
