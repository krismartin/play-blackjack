module CardImage exposing (url)

import Cards

resolveSuit : Cards.Suit -> String
resolveSuit suit =
  case suit of
    Cards.Spades ->
      "S"
    Cards.Hearts ->
      "H"
    Cards.Diamonds ->
      "D"
    Cards.Clubs ->
      "C"

resolveFace : Cards.Face -> String
resolveFace face =
  case face of
    Cards.Ace ->
      "A"

    Cards.Two ->
      "2"

    Cards.Three ->
      "3"

    Cards.Four ->
      "4"

    Cards.Five ->
      "5"

    Cards.Six ->
      "6"

    Cards.Seven ->
      "7"

    Cards.Eight ->
      "8"

    Cards.Nine ->
      "9"

    Cards.Ten ->
      "10"

    Cards.Jack ->
      "J"

    Cards.Queen ->
      "Q"

    Cards.King ->
      "K"

url : Cards.Card -> String
url card =
  case card of
      Cards.Card suit face ->
        "https://deckofcardsapi.com/static/img/"
          ++ resolveFace face
          ++ resolveSuit suit
          ++ ".png"

      Cards.Back ->
        "https://deckofcardsapi.com/static/img/back.png"
