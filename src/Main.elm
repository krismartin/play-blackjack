module Main exposing (..)

import Browser
import Cards
import Chip exposing (Chip)
import Deck
import Games.Blackjack exposing (score)
import Html exposing (Html, a, button, div, h1, span, text)
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
    | NoWinner


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
    , handsPlayed : Int
    , handsWon : Int
    , highestBank : Int
    , highestWinning : Int
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
    , winner = NoWinner
    , handsPlayed = 0
    , handsWon = 0
    , highestBank = initialBank
    , highestWinning = 0
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
    | UndoBet Chip
    | UpdateChips
    | UpdateGameStats Int
    | RestartGame


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


calculateWinning : Winner -> Int -> Int -> Int
calculateWinning winner betValue playerScore =
    let
        winning =
            if winner == Bettor then
                if playerScore == maxScore then
                    -- Blackjack pays 3 to 2
                    (betValue // 2) * 3

                else
                    betValue

            else
                0
    in
    winning


determineWinner : Phase -> Int -> Int -> Winner
determineWinner phase dealerScore playerScore =
    let
        winner =
            if playerScore == maxScore then
                -- Player scores blackjack
                Bettor

            else if playerScore > maxScore then
                -- Player busted
                Dealer

            else if dealerScore > maxScore then
                -- Dealer busted
                Bettor

            else if phase == PlayerStand then
                if dealerScore == maxScore then
                    -- Dealer scores blackjack
                    Dealer

                else if dealerScore < 17 then
                    -- Draw another card to the dealer's hand
                    NoWinner

                else if playerScore > dealerScore then
                    -- Player wins
                    Bettor

                else if dealerScore > playerScore then
                    -- Dealer wins
                    Dealer

                else if dealerScore == playerScore then
                    -- It's a draw
                    Draw

                else
                    NoWinner

            else
                NoWinner
    in
    winner


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckScores ->
            let
                ( dealer, player ) =
                    model.players

                winner =
                    determineWinner model.phase dealer.score player.score

                phase =
                    if winner == NoWinner then
                        model.phase

                    else
                        Ended

                betValue =
                    totalBet model.bets

                totalWinning =
                    calculateWinning winner betValue player.score

                bank =
                    model.bank
                        -- Add winning to bank
                        + totalWinning
                        -- Return bet if player wins or if it's a draw
                        + (if winner == Bettor || winner == Draw then
                            betValue

                           else
                            0
                          )

                bets =
                    if winner == NoWinner then
                        model.bets

                    else
                        []
            in
            ( { model
                | phase = phase
                , winner = winner
                , bank = bank
                , bets = bets
              }
            , if phase == Ended then
                -- Shuffle deck if game has ended
                Random.generate ShuffleDeck Deck.randomDeck

              else
                Cmd.none
            )
                |> Update.Extra.andThen update (UpdateGameStats totalWinning)
                |> Update.Extra.andThen update UpdateChips
                |> Update.Extra.filter (phase == PlayerStand)
                    (Update.Extra.andThen update DealerDraw)

        Deal ->
            let
                { handsPlayed, players } =
                    model

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
                    players
            in
            ( { model
                | deck = deck
                , players =
                    ( { dealer | dealer = True, hand = dealerCards, score = dealerScore }
                    , { player | hand = playerCards, score = playerScore }
                    )
                , phase = PlayerDeal
                , winner = NoWinner
                , handsPlayed = handsPlayed + 1
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
                { bank, bets } =
                    model

                nextBank =
                    bank - chip.value
            in
            ( { model
                | bank = nextBank
                , bets = bets ++ [ chip ]
              }
            , Cmd.none
            )
                |> Update.Extra.andThen update UpdateChips

        UndoBet lastBet ->
            let
                bets =
                    List.reverse model.bets
                        |> List.drop 1
                        |> List.reverse
            in
            ( { model | bets = bets, bank = model.bank + lastBet.value }, Cmd.none )
                |> Update.Extra.andThen update UpdateChips

        UpdateChips ->
            let
                chips =
                    List.map (\c -> { c | active = c.value <= model.bank }) model.chips
            in
            ( { model | chips = chips }, Cmd.none )

        UpdateGameStats totalWinning ->
            let
                handsWon =
                    if model.winner == Bettor then
                        model.handsWon + 1

                    else
                        model.handsWon

                highestBank =
                    if model.bank > model.highestBank then
                        model.bank

                    else
                        model.highestBank

                highestWinning =
                    if totalWinning > model.highestWinning then
                        totalWinning

                    else
                        model.highestWinning
            in
            ( { model
                | handsWon = handsWon
                , highestBank = highestBank
                , highestWinning = highestWinning
              }
            , Cmd.none
            )

        RestartGame ->
            ( initialModel, Random.generate ShuffleDeck Deck.randomDeck )



-- VIEW


view : Model -> Html Msg
view model =
    let
        { bank, bets, phase, players, handsPlayed, handsWon, highestBank, highestWinning } =
            model

        ( dealer, player ) =
            players

        houseWon =
            phase == Ended && bank == 0 && (List.isEmpty bets == True)
    in
    div [ class "main" ]
        [ viewHand dealer phase
        , viewGameStatus model
        , viewHand player phase
        , viewPlayerControl phase bets
        , viewChips model
        , if houseWon == True then
            viewGameStats handsPlayed handsWon highestWinning highestBank

          else
            text ""
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
            [ viewBets bets
            , button [ class "button--important", onClick Deal, disabled (List.isEmpty bets) ] [ text "Deal" ]
            ]

    else
        div [ class "player-control" ]
            [ button [ class "button--important", onClick Hit ] [ text "Hit" ]
            , button [ onClick Stand ] [ text "Stand" ]
            ]


viewBets : Chips -> Html Msg
viewBets bets =
    let
        mostRecentBet =
            case List.reverse bets |> List.head of
                Nothing ->
                    div [ class "chip" ] []

                Just chip ->
                    Chip.view [ onClick (UndoBet chip) ] chip
    in
    div
        [ class "bets" ]
        [ mostRecentBet
        , div
            [ class "total-bet" ]
            [ text ("Bet: $" ++ String.fromInt (totalBet bets)) ]
        ]


viewChips : AppData -> Html Msg
viewChips model =
    let
        { bank, phase } =
            model

        allowBetting =
            phase == Waiting || phase == Ended

        bankAndBet =
            [ span [ class "bank" ] [ text ("Bank: $" ++ String.fromInt bank) ]
            , if allowBetting == False then
                span [ class "bet" ] [ text ("Bet: $" ++ String.fromInt (totalBet model.bets)) ]

              else
                text ""
            ]
    in
    div
        [ class "player-wallet" ]
        [ div
            [ class "drawer" ]
            [ div [ class "player-bank" ] bankAndBet
            , if allowBetting == True then
                div [ class "player-chips" ] (List.map viewChip model.chips)

              else
                text ""
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


viewGameStats : Int -> Int -> Int -> Int -> Html Msg
viewGameStats handsPlayed handsWon highestWinning highestBank =
    div
        [ class "modal-window" ]
        [ div
            [ class "game-stats" ]
            [ h1 [] [ text "House wins!" ]
            , div
                [ class "game-stat" ]
                [ div [] [ text "Hands played" ]
                , div [] [ text (String.fromInt handsPlayed) ]
                ]
            , div
                [ class "game-stat" ]
                [ div [] [ text "Hands won" ]
                , div [] [ text (String.fromInt handsWon) ]
                ]
            , div
                [ class "game-stat" ]
                [ div [] [ text "Highest winning" ]
                , div [] [ text ("$" ++ String.fromInt highestWinning) ]
                ]
            , div
                [ class "game-stat" ]
                [ div [] [ text "Highest bank" ]
                , div [] [ text ("$" ++ String.fromInt highestBank) ]
                ]
            , div
                [ class "actions", Html.Attributes.style "padding-top" "20px" ]
                [ button [ onClick RestartGame, class "button--important" ] [ text "Continue" ] ]
            ]
        ]
