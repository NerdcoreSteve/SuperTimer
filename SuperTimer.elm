{-
TODO
get current and break time working
make start/pause button into work/stop-working button
break timer instead of owed. It counts down. working increments it.
keyboard controls
instructions for user
always double digit
style it up
Pheonix back-end that lets you auth and remembers this state.
customize block time
customize break rewards
figure out how to trello-ize this
???
-}
import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import StartApp
import Effects
import Signal exposing (Address)

type alias Model = 
    {
        seconds : Int,
        minutes : Int,
        running : Bool,
        current_block: Int,
        break_time_owed: Int
    }

type Action = Increment | StartPause | Reset

init : Model 
init = Model 0 0 False 1 0

update: Action -> Model -> Model
update action model =
    case action of 
        Increment -> 
            if model.running then
                if model.seconds < 60 then
                    { model | seconds = model.seconds + 1 }
                else
                    if model.minutes < 25 then
                        { model | minutes = model.minutes + 1, seconds = 0 }
                    else
                        { model | running = False }
            else
                model
        StartPause -> 
            { model | running = not model.running }
        Reset -> 
            { model | seconds = 0, minutes = 0, running = False }


view : Address Action -> Model -> Html
view address model =
    div 
        []
        [
            div [] [text ("time: " ++ (toString model.minutes) ++ ":" ++ (toString model.seconds))],
            div [] [text ("current block: " ++ (toString model.current_block))],
            div [] [text ("break time owed: " ++ (toString model.break_time_owed))],
            div
                []
                [
                    button
                        [ onClick address StartPause ]
                        [ text <| if model.running then "Pause" else "Start" ],
                    button 
                        [onClick address Reset ]
                        [ text "Reset" ]
                ]
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
