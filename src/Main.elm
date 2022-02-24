module Main exposing (..)

import Browser
import Cards
import Deck
import Games.Blackjack exposing (score)
import Html exposing (Html, button, div, img, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Hand = List Cards.Card

type alias AppData =
  { deck : Deck.ShuffledDeck
  , dealerHand : Hand
  , playerHand : Hand
  , dealerScore : Int
  , playerScore : Int
  }

type alias Model = AppData

deck : Deck.ShuffledDeck
deck = Deck.fullDeck

init : Model
init =
  { deck = deck
  , dealerHand = []
  , playerHand = []
  , dealerScore = 0
  , playerScore = 0
  }


-- UPDATE

type Msg =
  StartGame
  | Hit
  | Stand

update : Msg -> Model -> Model
update msg model =
  case msg of
    StartGame ->
      let
        (drawnCard1, newDeck) = Deck.draw model.deck
        (drawnCard2, newDeck2) = Deck.draw newDeck
        (drawnCard3, newDeck3) = Deck.draw newDeck2
        (drawnCard4, newDeck4) = Deck.draw newDeck3
        dealerCards = [ drawnCard1, drawnCard2 ]
        dealerScore = score(Deck.newDeck dealerCards)
        playerCards = [ drawnCard3, drawnCard4 ]
        playerScore = score(Deck.newDeck playerCards)
      in
        { model |
          deck = newDeck4
          , dealerHand = dealerCards
          , playerHand = playerCards
          , dealerScore = dealerScore
          , playerScore = playerScore
        }
    Hit ->
      let
        (drawnCard, newDeck) = Deck.draw model.deck
        playerCards = List.append model.playerHand [ drawnCard ]
        playerScore = score(Deck.newDeck playerCards)
      in
        { model |
          deck = newDeck
          , playerHand = playerCards
          , playerScore = playerScore
        }
    Stand ->
      model


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [] [
      text ("Dealer's hand (" ++ String.fromInt model.dealerScore ++ "):")
      , div[] (List.map viewCard model.dealerHand)
      ]
    , div [] [
      text ("Player's hand (" ++ String.fromInt model.playerScore ++ "):")
      , div[] (List.map viewCard model.playerHand)
      ]
    , viewControls model
    ]

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

viewCard : Cards.Card -> Html Msg
viewCard card =
  case card of
      Cards.Card suit face ->
        let
          imageUrl = "https://deckofcardsapi.com/static/img/"
            ++ resolveFace face
            ++ resolveSuit suit
            ++ ".png"
        in
          img [ src imageUrl ] []

      Cards.Back ->
        text ""

viewControls : Model -> Html Msg
viewControls model =
  if List.isEmpty model.dealerHand == True then
    div [] [ button [ onClick StartGame ] [ text "Start Game"] ]
  else
    div []
      [ button [onClick Hit ] [ text "Hit"]
      , button [onClick Stand ] [ text "Stand"]
      ]


