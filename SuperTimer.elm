import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import StartApp
import Effects
import Signal exposing (Address)

type alias Model = 
    {
        seconds : Int,
        isRunning : Bool
    }

type Action = Increment | StartPause | Reset

init : Model 
init = Model 0 False

update: Action -> Model -> Model
update action model =
    case action of 
        Increment -> 
            if model.isRunning && model.seconds < 25
                then { model | seconds = model.seconds + 1 }
                else model
        StartPause -> 
            { model | isRunning = not model.isRunning }
        Reset -> 
            { model | seconds = 0 }


view : Address Action -> Model -> Html
view address model =
    div 
        []
        [
            (text << toString) model.seconds,
            button
                [ onClick address StartPause ]
                [ text <| if model.isRunning then "Pause Timer" else "Start Timer" ],
            button 
                [onClick address Reset ]
                [ text "Reset Timer" ]
        ]

app : StartApp.App Model
app = StartApp.start 
    {
        init = (init, Effects.none),
        update = (\address model -> (update address model, Effects.none)),
        view = view,
        inputs = [Signal.map (\_ -> Increment) (every second)]
    }

main : Signal Html
main = app.html
