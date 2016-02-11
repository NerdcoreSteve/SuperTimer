module SuperTimer where

import Html
import Time


view : Int -> Html.Html
view count =
  Html.text (toString count)


countSignal : Signal Int
countSignal =
    Signal.foldp (\_ state -> state + 1) 0 (Time.every Time.second)


main : Signal.Signal Html.Html
main =
  Signal.map view countSignal
