import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp

main = StartApp.start { model = model, view = view, update = update }

model = ""

view address model =
  div 
    []
    [ 
        button [ onClick address Stop ] [ text "stop" ],
        div [] [ text (toString model) ],
        button [ onClick address Start ] [ text "start" ]
    ]

type Action = Start | Stop

update action model =
  case action of
    Start -> "Starting..." 
    Stop -> "Stopping..."
