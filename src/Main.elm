module Main exposing (..)

import Browser
import CardImage
import Cards
import Deck
import Element as El exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Games.Blackjack exposing (score)
import Html.Attributes
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

                    else if dealerScore == maxScore then
                        -- Dealer scores blackjack
                        ( Ended, Dealer )

                    else if playerScore > maxScore then
                        -- Player busted
                        ( Ended, Dealer )

                    else if dealerScore > maxScore then
                        -- Dealer busted
                        ( Ended, Bettor )

                    else if model.phase == PlayerStand then
                        if dealerScore < 17 then
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


view : Model -> Browser.Document Msg
view model =
    let
        ( dealer, player ) =
            model.players
    in
    { title = "Play Blackjack"
    , body =
        [ El.layout
            [ El.htmlAttribute (Html.Attributes.style "background" "url(./public/assets/bg.png) no-repeat center #005b13")
            , Font.family theme.fontFamily
            , El.width El.fill
            , El.height El.fill
            ]
            (El.column
                [ El.height El.fill
                , El.centerX
                , El.centerY
                ]
                [ viewHand dealer model.phase
                , viewWinner model.winner model.players
                , viewHand player model.phase
                , viewControls model
                ]
            )
        ]
    }


viewHand : Player -> Phase -> Element Msg
viewHand player phase =
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

        verticalAlignment =
            if player.dealer then
                "center"

            else
                "flex-end"
    in
    if List.isEmpty player.hand then
        El.row
            [ El.height El.fill ]
            []

    else
        El.row
            [ El.height El.fill
            , El.padding 40
            , El.centerX
            , El.htmlAttribute (Html.Attributes.style "align-items" verticalAlignment)
            ]
            (List.map viewCard cards)


viewCard : Cards.Card -> Element Msg
viewCard card =
    El.image
        [ El.paddingEach { top = 0, right = 10, bottom = 0, left = 0 }
        , El.height (El.px 180)
        ]
        { description = "", src = CardImage.url card }


viewWinner : Winner -> ( Player, Player ) -> Element Msg
viewWinner winner ( dealer, player ) =
    let
        colors =
            { playerWin = [ Background.color (El.rgb255 255 234 0) ]
            , dealerWin = [ Background.color (El.rgb255 210 55 55), Font.color (El.rgb255 255 255 255) ]
            , draw = [ Background.color (El.rgb255 23 175 34) ]
            }

        ( color, text ) =
            if winner == Bettor then
                ( colors.playerWin
                , if player.score == maxScore then
                    "Blackjack!"

                  else
                    "You win!"
                )

            else if winner == Dealer then
                ( colors.dealerWin
                , if dealer.score == maxScore then
                    "Dealer wins (Blackjack)"

                  else if player.score > maxScore then
                    "Busted"

                  else
                    "Dealer wins"
                )

            else if winner == Draw then
                ( colors.draw, "Draw" )

            else
                ( [], "" )

        styles =
            color
                ++ [ El.htmlAttribute (Html.Attributes.style "margin" "0 auto")
                   , El.padding 20
                   , Border.rounded theme.button.radius
                   ]
    in
    if text == "" then
        El.row
            [ El.width El.fill
            , El.htmlAttribute (Html.Attributes.style "min-height" "180px")
            , El.htmlAttribute (Html.Attributes.style "align-items" "flex-start")
            ]
            []

    else
        El.row
            [ El.width El.fill
            , El.height El.fill
            , El.centerX
            , El.htmlAttribute (Html.Attributes.style "min-height" "180px")
            , El.htmlAttribute (Html.Attributes.style "align-items" "flex-start")
            ]
            [ El.el styles (El.text text) ]


viewControls : Model -> Element Msg
viewControls model =
    let
        player =
            Tuple.first model.players
    in
    if model.phase == Waiting || model.phase == Ended then
        El.row
            [ El.height El.fill
            , El.paddingEach { top = 0, right = 0, bottom = 40, left = 0 }
            , El.centerX
            , El.htmlAttribute (Html.Attributes.style "align-items" "flex-end")
            , El.htmlAttribute (Html.Attributes.style "max-height" "80px")
            ]
            [ Input.button Theme.button { label = El.text "Deal", onPress = Just Deal }
            ]

    else
        El.row
            [ El.width El.fill
            , El.height El.fill
            , El.paddingEach { top = 0, right = 0, bottom = 40, left = 0 }
            , El.centerX
            , El.htmlAttribute (Html.Attributes.style "justify-content" "center")
            , El.htmlAttribute (Html.Attributes.style "align-items" "flex-end")
            , El.htmlAttribute (Html.Attributes.style "max-height" "80px")
            ]
            [ Input.button Theme.button { label = El.text "Hit", onPress = Just Hit }
            , El.el [ El.paddingEach { top = 0, right = 10, bottom = 0, left = 10 } ] El.none
            , Input.button Theme.button { label = El.text "Stand", onPress = Just Stand }
            ]
