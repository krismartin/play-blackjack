module Main exposing (..)

import Browser
import CardImage
import Cards
import Deck
import Games.Blackjack exposing (score)
import Html exposing (Html, button, div, img, span, text)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Random.generate ShuffleDeck Deck.randomDeck )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Hand =
    List Cards.Card


type alias Player =
    { hand : Hand
    , score : Int
    }


type alias AppData =
    { deck : Deck.ShuffledDeck
    , stand : Bool
    , players : ( Player, Player )
    }


type alias Model =
    AppData


initialModel : Model
initialModel =
    { deck = Deck.fullDeck
    , stand = False
    , players =
        ( { hand = [], score = 0 }
        , { hand = [], score = 0 }
        )
    }



-- UPDATE


type Msg
    = Deal
    | ShuffleDeck Deck.ShuffledDeck
    | Hit
    | Stand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Deal ->
            let
                ( drawnCard1, newDeck ) =
                    Deck.draw model.deck

                ( drawnCard2, newDeck2 ) =
                    Deck.draw newDeck

                ( drawnCard3, newDeck3 ) =
                    Deck.draw newDeck2

                ( drawnCard4, newDeck4 ) =
                    Deck.draw newDeck3

                dealerCards =
                    [ drawnCard1, drawnCard2 ]

                dealerScore =
                    score (Deck.newDeck dealerCards)

                playerCards =
                    [ drawnCard3, drawnCard4 ]

                playerScore =
                    score (Deck.newDeck playerCards)
            in
            ( { model
                | deck = newDeck4
                , players =
                    ( { hand = dealerCards, score = dealerScore }
                    , { hand = playerCards, score = playerScore }
                    )
                , stand = False
              }
            , Cmd.none
            )

        ShuffleDeck deck ->
            ( { model | deck = deck }, Cmd.none )

        Hit ->
            let
                ( drawnCard, newDeck ) =
                    Deck.draw model.deck

                dealer =
                    Tuple.first model.players

                player =
                    Tuple.second model.players

                playerCards =
                    List.append player.hand [ drawnCard ]

                playerScore =
                    score (Deck.newDeck playerCards)
            in
            ( { model
                | deck = newDeck
                , players =
                    ( dealer
                    , { hand = playerCards, score = playerScore }
                    )
              }
            , Cmd.none
            )

        Stand ->
            ( { model | stand = True }
            , Random.generate ShuffleDeck Deck.randomDeck
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        dealer =
            Tuple.first model.players

        player =
            Tuple.second model.players
    in
    div []
        [ viewHand "Dealer's hand:" dealer { hideFirstCard = model.stand == False }
        , viewHand "Your hand:" player { hideFirstCard = False }
        , viewControls model
        ]


type alias ViewHandOpts =
    { hideFirstCard : Bool }


viewHand : String -> Player -> ViewHandOpts -> Html Msg
viewHand label player opts =
    let
        backCard =
            [ Cards.defaultNew Cards.Back "back" 0 ]

        cards =
            if opts.hideFirstCard == True then
                backCard ++ List.drop 1 player.hand

            else
                player.hand
    in
    if List.isEmpty player.hand then
        text ""

    else
        div []
            [ text (label ++ " (score: " ++ String.fromInt player.score ++ ")")
            , div [] (List.map viewCard cards)
            ]


viewCard : Cards.Card -> Html Msg
viewCard card =
    span []
        [ img [ src (CardImage.url card) ] []
        ]


viewControls : Model -> Html Msg
viewControls model =
    let
        dealer =
            Tuple.first model.players
    in
    if model.stand == True then
        div [] [ button [ onClick Deal ] [ text "Deal" ] ]

    else if List.isEmpty dealer.hand == True then
        div [] [ button [ onClick Deal ] [ text "Deal" ] ]

    else
        div []
            [ button [ onClick Hit ] [ text "Hit" ]
            , button [ onClick Stand ] [ text "Stand" ]
            ]
