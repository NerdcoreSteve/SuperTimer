module SuperTimer where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = Int

-- INIT
init : Int
init = 0

-- UPDATE

type Action = Increment

update : Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "Start Timer" ]
    ]
