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
import Update.Extra



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
    | Ended


type Winner
    = Dealer
    | Bettor
    | Draw
    | Nothing


type alias AppData =
    { deck : Deck.ShuffledDeck
    , players : ( Player, Player )
    , phase : Phase
    , winner : Winner
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
    , winner = Nothing
    }



-- UPDATE


type Msg
    = Deal
    | ShuffleDeck Deck.ShuffledDeck
    | CheckScores
    | Hit
    | Stand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckScores ->
            let
                ( dealer, player ) =
                    model.players

                dealerScore =
                    dealer.score

                playerScore =
                    player.score

                ( nextPhase, nextWinner ) =
                    if playerScore == 21 then
                        ( Ended, Bettor )

                    else if dealerScore == 21 then
                        ( Ended, Dealer )

                    else if playerScore > 21 then
                        ( Ended, Dealer )

                    else if dealerScore > 21 then
                        ( Ended, Bettor )

                    else if model.phase == PlayerStand then
                        if playerScore > dealerScore then
                            ( Ended, Bettor )

                        else if dealerScore > playerScore then
                            ( Ended, Dealer )

                        else if dealerScore == playerScore then
                            ( Ended, Draw )

                        else
                            ( model.phase, model.winner )

                    else
                        ( model.phase, model.winner )

                nextCmd =
                    if nextPhase == Ended then
                        Random.generate ShuffleDeck Deck.randomDeck

                    else
                        Cmd.none
            in
            ( { model | phase = nextPhase, winner = nextWinner }, nextCmd )

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
                , winner = Nothing
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update CheckScores

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
                |> Update.Extra.andThen update CheckScores

        Stand ->
            ( { model | phase = PlayerStand }, Cmd.none )
                |> Update.Extra.andThen update CheckScores



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
        , viewWinner model.winner
        , viewHand "Your hand:" player model.phase
        , viewControls model
        ]


viewHand : String -> Player -> Phase -> Html Msg
viewHand label player phase =
    let
        backCard =
            [ Cards.defaultNew Cards.Back "back" 0 ]

        hideFirstCard =
            if phase == PlayerStand || phase == Ended then
                False

            else
                player.dealer

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


viewWinner : Winner -> Html Msg
viewWinner winner =
    if winner == Bettor then
        div [] [ text "You won!" ]

    else if winner == Dealer then
        div [] [ text "Dealer wins" ]

    else if winner == Draw then
        div [] [ text "Draw" ]

    else
        text ""


viewControls : Model -> Html Msg
viewControls model =
    if model.phase == Waiting || model.phase == Ended then
        div [] [ button [ onClick Deal ] [ text "Deal" ] ]

    else
        div []
            [ button [ onClick Hit ] [ text "Hit" ]
            , button [ onClick Stand ] [ text "Stand" ]
            ]
