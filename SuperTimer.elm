module SuperTimer where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


-- MODEL

type alias Model = String


-- UPDATE

type Action = Hello

update : Action -> Model -> Model
update action model =
  case action of
    Hello -> "Hey There!"


-- VIEW

view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ div [] [ text model ]
    , button [ onClick address Hello ] [ text "Press it!" ]
    ]
