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
    { dealer : Bool
    , hand : Hand
    , score : Int
    }


type Phase
    = Waiting
    | PlayerDeal
    | PlayerHit
    | PlayerStand
    | DealerWin
    | PlayerWin


type alias AppData =
    { deck : Deck.ShuffledDeck
    , players : ( Player, Player )
    , phase : Phase
    }


type alias Model =
    AppData


initialModel : Model
initialModel =
    { deck = Deck.fullDeck
    , phase = Waiting
    , players =
        ( { dealer = True, hand = [], score = 0 }
        , { dealer = False, hand = [], score = 0 }
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

                ( dealer, player ) =
                    model.players
            in
            ( { model
                | deck = newDeck4
                , players =
                    ( { dealer | dealer = True, hand = dealerCards, score = dealerScore }
                    , { player | hand = playerCards, score = playerScore }
                    )
                , phase = PlayerDeal
              }
            , Cmd.none
            )

        ShuffleDeck deck ->
            ( { model | deck = deck }, Cmd.none )

        Hit ->
            let
                ( drawnCard, newDeck ) =
                    Deck.draw model.deck

                playerCards =
                    List.append player.hand [ drawnCard ]

                playerScore =
                    score (Deck.newDeck playerCards)

                ( dealer, player ) =
                    model.players
            in
            ( { model
                | deck = newDeck
                , players =
                    ( dealer
                    , { player | hand = playerCards, score = playerScore }
                    )
                , phase = PlayerHit
              }
            , Cmd.none
            )

        Stand ->
            ( { model | phase = PlayerStand }
            , Random.generate ShuffleDeck Deck.randomDeck
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        _ =
            Debug.log "Phase" model.phase

        ( dealer, player ) =
            model.players
    in
    div []
        [ viewHand "Dealer's hand:" dealer model.phase
        , viewHand "Your hand:" player model.phase
        , viewControls model
        ]


viewHand : String -> Player -> Phase -> Html Msg
viewHand label player phase =
    let
        backCard =
            [ Cards.defaultNew Cards.Back "back" 0 ]

        hideFirstCard =
            player.dealer == True && phase /= PlayerStand

        cards =
            if hideFirstCard then
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
    if model.phase == Waiting || model.phase == PlayerStand then
        div [] [ button [ onClick Deal ] [ text "Deal" ] ]

    else
        div []
            [ button [ onClick Hit ] [ text "Hit" ]
            , button [ onClick Stand ] [ text "Stand" ]
            ]
