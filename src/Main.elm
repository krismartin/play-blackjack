module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

-- MAIN
main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias AppData =
  { deckId : String }

type alias Model = AppData

init : Model
init =
  { deckId = "" }


-- UPDATE

type Msg = NewGame

update : Msg -> Model -> Model
update msg model =
  case msg of
    NewGame ->
      { model | deckId = "[TODO] Shuffle decks and get Deck ID" }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick NewGame ] [ text "New Game" ]
    , div [] [ text (model.deckId) ]
    ]
