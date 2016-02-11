module SuperTimer where

import Html exposing (..)
import Time

view : Int -> Html.Html
view count =
    div []
    [ div [] [ text (toString count) ]
    , button [] [ text "Start Counter" ]
    ]

timer_signal : Signal Int
timer_signal =
    Signal.foldp (\_ state -> state + 1) 0 (Time.every Time.second)


main : Signal.Signal Html.Html
main =
    Signal.map view timer_signal
