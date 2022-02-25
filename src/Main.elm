module Main exposing (..)

import Browser
import CardImage
import Cards
import Deck
import Element as El exposing (Element)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Games.Blackjack exposing (score)
import Random
import Theme exposing (theme)
import Update.Extra



-- MAIN


main : Program () Model Msg
main =
    Browser.document
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


view : Model -> Browser.Document Msg
view model =
    let
        _ =
            Debug.log "Phase" model.phase

        ( dealer, player ) =
            model.players
    in
    { title = "Play Blackjack"
    , body =
        [ El.layout
            [ Background.gradient { angle = 3.1, steps = [ El.rgb255 26 74 28, El.rgb255 48 136 52 ] }
            , Font.family theme.fontFamily
            ]
            (El.column
                [ El.height El.fill
                , El.centerX
                , El.centerY
                ]
                [ viewHand "Dealer's hand:" dealer model.phase
                , viewWinner model.winner model.players
                , viewHand "Your hand:" player model.phase
                , viewControls model
                ]
            )
        ]
    }


viewHand : String -> Player -> Phase -> Element Msg
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
        El.none

    else
        El.row [ El.padding 40, El.centerX ]
            [ El.row [] (List.map viewCard cards) ]


viewCard : Cards.Card -> Element Msg
viewCard card =
    El.row [ El.paddingEach { top = 0, right = 10, bottom = 0, left = 0 } ]
        [ El.image [] { description = "", src = CardImage.url card }
        ]


viewWinner : Winner -> ( Player, Player ) -> Element Msg
viewWinner winner ( dealer, player ) =
    let
        styles =
            { playerWin = [ Background.color (El.rgb255 255 234 0) ]
            , dealerWin = [ Background.color (El.rgb255 210 55 55), Font.color (El.rgb255 255 255 255) ]
            , draw = [ Background.color (El.rgb255 23 175 34) ]
            }

        ( style, text ) =
            if winner == Bettor then
                ( styles.playerWin, "You win!" )

            else if winner == Dealer then
                ( styles.dealerWin, "Dealer wins" )

            else if winner == Draw then
                ( styles.draw, "Draw" )

            else
                ( [], "" )
    in
    if text == "" then
        El.none

    else
        El.row
            (Theme.notice ++ style)
            [ El.text text ]


viewControls : Model -> Element Msg
viewControls model =
    if model.phase == Waiting || model.phase == Ended then
        El.row [ El.padding 20, El.centerX ] [ Input.button Theme.button { label = El.text "Deal", onPress = Just Deal } ]

    else
        El.row [ El.padding 20, El.centerX ]
            [ Input.button Theme.button { label = El.text "Hit", onPress = Just Hit }
            , El.text "   "
            , Input.button Theme.button { label = El.text "Stand", onPress = Just Stand }
            ]
