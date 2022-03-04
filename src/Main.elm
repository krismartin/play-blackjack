module Main exposing (..)

import Browser
import Cards
import Deck
import Games.Blackjack exposing (score)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import PlayingCard
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


maxScore : Int
maxScore =
    21


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
    | DealerDraw


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
                    if playerScore == maxScore then
                        -- Player scores blackjack
                        ( Ended, Bettor )

                    else if playerScore > maxScore then
                        -- Player busted
                        ( Ended, Dealer )

                    else if dealerScore > maxScore then
                        -- Dealer busted
                        ( Ended, Bettor )

                    else if model.phase == PlayerStand then
                        if dealerScore == maxScore then
                            -- Dealer scores blackjack
                            ( Ended, Dealer )

                        else if dealerScore < 17 then
                            -- Draw another card to the dealer's hand
                            ( model.phase, model.winner )

                        else if playerScore > dealerScore then
                            -- Player wins
                            ( Ended, Bettor )

                        else if dealerScore > playerScore then
                            -- Dealer wins
                            ( Ended, Dealer )

                        else if dealerScore == playerScore then
                            -- It's a draw
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
                |> Update.Extra.filter (nextPhase == PlayerStand)
                    (Update.Extra.andThen update DealerDraw)

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

        DealerDraw ->
            let
                ( dealer, player ) =
                    model.players

                ( drawnCard, nextDeck ) =
                    Deck.draw model.deck

                dealerCards =
                    List.append dealer.hand [ drawnCard ]

                dealerScore =
                    score (Deck.newDeck dealerCards)

                drawAnotherCard =
                    model.phase == PlayerStand && dealerScore < 17
            in
            ( { model | players = ( { dealer | hand = dealerCards, score = dealerScore }, player ), deck = nextDeck }, Cmd.none )
                |> Update.Extra.filter (drawAnotherCard == True)
                    (Update.Extra.andThen update Stand)
                |> Update.Extra.filter (drawAnotherCard == False)
                    (Update.Extra.andThen update CheckScores)



-- VIEW


view : Model -> Html Msg
view model =
    let
        { phase, players } =
            model

        ( dealer, player ) =
            players
    in
    div [ class "main" ]
        [ viewHand dealer phase
        , viewGameStatus model
        , viewHand player phase
        , viewPlayerControl phase
        ]


viewPlayerControl : Phase -> Html Msg
viewPlayerControl phase =
    if phase == Waiting || phase == Ended then
        div [ class "player-control" ]
            [ button [ class "button--important", onClick Deal ] [ text "Deal" ] ]

    else
        div [ class "player-control" ]
            [ button [ class "button--important", onClick Hit ] [ text "Hit" ]
            , button [ onClick Stand ] [ text "Stand" ]
            ]


viewGameStatus : AppData -> Html Msg
viewGameStatus model =
    let
        { phase, winner, players } =
            model
    in
    if phase == Ended then
        div [ class "game-panel" ]
            [ viewWinner winner players ]

    else
        div [ class "game-panel" ]
            [ text "" ]


viewHand : Player -> Phase -> Html Msg
viewHand player phase =
    let
        backCard =
            [ Cards.defaultNew Cards.Back "back" 0 ]

        { dealer, hand } =
            player

        hideFirstCard =
            if phase == PlayerStand || phase == Ended then
                False

            else
                dealer

        cards =
            if hideFirstCard then
                backCard ++ List.drop 1 hand

            else
                hand

        classes =
            [ ( "player-hand", True ), ( "player-hand--dealer", dealer == True ) ]
    in
    if List.isEmpty hand == True then
        div [ classList classes ] []

    else
        div [ classList classes ] (List.map viewCard cards)


viewCard : Cards.Card -> Html Msg
viewCard card =
    div [] [ PlayingCard.view card ]


viewWinner : Winner -> ( Player, Player ) -> Html Msg
viewWinner winner ( dealer, player ) =
    let
        description =
            if winner == Bettor then
                if player.score == maxScore then
                    "Blackjack!"

                else
                    "You win!"

            else if winner == Dealer then
                if player.score > maxScore then
                    "Busted"

                else
                    "Dealer wins"

            else if winner == Draw then
                "It's a draw"

            else
                ""
    in
    div [ class "game-status" ]
        [ div
            [ classList
                [ ( "winner", True )
                , ( "winner--player", winner == Bettor )
                , ( "winner--dealer", winner == Dealer )
                , ( "winner--draw", winner == Draw )
                ]
            ]
            [ text description ]
        ]
