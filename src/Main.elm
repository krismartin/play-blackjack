module Main exposing (..)

import Browser
import Cards
import Chip exposing (Chip)
import Deck
import Games.Blackjack exposing (score)
import Html exposing (Html, a, button, div, text)
import Html.Attributes exposing (class, classList, disabled)
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


type alias Chips =
    List Chip


type alias AppData =
    { bank : Int
    , bets : Chips
    , chips : Chips
    , deck : Deck.ShuffledDeck
    , players : ( Player, Player )
    , phase : Phase
    , winner : Winner
    }


type alias Model =
    AppData


maxScore : Int
maxScore =
    21


initialBank : Int
initialBank =
    500


initialChips : List Chip
initialChips =
    [ Chip.newChip 5 "#db2929"
    , Chip.newChip 10 "#073d91"
    , Chip.newChip 25 "#168716"
    , Chip.newChip 50 "#ed9900"
    , Chip.newChip 100 "#000000"
    , Chip.newChip 500 "#9f249f"
    ]


initialModel : Model
initialModel =
    { bank = initialBank
    , bets = []
    , chips = initialChips
    , deck = Deck.fullDeck
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
    | AddBet Chip


type alias DealCards =
    { drawnCards : List Cards.Card
    , deck : Deck.ShuffledDeck
    }


dealCards : Int -> Deck.ShuffledDeck -> DealCards
dealCards count deck =
    List.foldl drawCard { deck = deck, drawnCards = [] } (List.repeat count 0)


drawCard : Int -> DealCards -> DealCards
drawCard _ state =
    let
        ( drawnCard, nextDeck ) =
            Deck.draw state.deck
    in
    { drawnCards = state.drawnCards ++ [ drawnCard ], deck = nextDeck }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckScores ->
            let
                { bank, bets, chips, players, phase, winner } =
                    model

                ( dealer, player ) =
                    players

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
                            ( phase, winner )

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
                            ( phase, winner )

                    else
                        ( phase, winner )

                nextCmd =
                    if nextPhase == Ended then
                        Random.generate ShuffleDeck Deck.randomDeck

                    else
                        Cmd.none

                betValue =
                    totalBet bets

                nextBank =
                    if nextWinner == Bettor then
                        bank + (betValue * 2)

                    else if nextWinner == Draw then
                        bank + betValue

                    else
                        bank

                nextBets =
                    if nextWinner == Nothing then
                        bets

                    else
                        []

                nextChips =
                    List.map (\c -> { c | active = c.value <= nextBank }) chips
            in
            ( { model | phase = nextPhase, winner = nextWinner, bank = nextBank, bets = nextBets, chips = nextChips }, nextCmd )
                |> Update.Extra.filter (nextPhase == PlayerStand)
                    (Update.Extra.andThen update DealerDraw)

        Deal ->
            let
                { drawnCards, deck } =
                    dealCards 4 model.deck

                dealerCards =
                    List.take 2 drawnCards

                dealerScore =
                    score (Deck.newDeck dealerCards)

                playerCards =
                    List.drop 2 drawnCards

                playerScore =
                    score (Deck.newDeck playerCards)

                ( dealer, player ) =
                    model.players
            in
            ( { model
                | deck = deck
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

        AddBet chip ->
            let
                { bank, bets, chips } =
                    model

                nextBank =
                    bank - chip.value

                nextChips =
                    List.map (\c -> { c | active = c.value <= nextBank }) chips
            in
            ( { model
                | bank = nextBank
                , bets = bets ++ [ chip ]
                , chips = nextChips
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        { bets, phase, players } =
            model

        ( dealer, player ) =
            players
    in
    div [ class "main" ]
        [ viewHand dealer phase
        , viewGameStatus model
        , viewHand player phase
        , viewPlayerControl phase bets
        , viewChips model
        ]


viewPlayerControl : Phase -> Chips -> Html Msg
viewPlayerControl phase bets =
    if phase == Waiting || phase == Ended then
        div
            [ classList
                [ ( "player-control", True )
                , ( "player-control--waiting", phase == Waiting )
                ]
            ]
            [ button [ class "button--important", onClick Deal, disabled (List.isEmpty bets) ] [ text "Deal" ]
            ]

    else
        div [ class "player-control" ]
            [ button [ class "button--important", onClick Hit ] [ text "Hit" ]
            , button [ onClick Stand ] [ text "Stand" ]
            ]


viewChips : AppData -> Html Msg
viewChips model =
    div
        [ class "player-wallet" ]
        [ div
            [ class "drawer" ]
            [ div [ class "player-bank" ] [ text ("Bank: $" ++ String.fromInt model.bank ++ ", Bet: $" ++ String.fromInt (totalBet model.bets)) ]
            , div [ class "player-chips" ] (List.map viewChip model.chips)
            ]
        ]


viewChip : Chip -> Html Msg
viewChip chip =
    let
        attributes =
            if chip.active == True then
                [ onClick (AddBet chip) ]

            else
                []
    in
    Chip.view attributes chip


totalBet : Chips -> Int
totalBet chips =
    List.foldl (+) 0 (List.map (\chip -> chip.value) chips)


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
