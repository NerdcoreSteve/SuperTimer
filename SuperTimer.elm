import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import StartApp
import Effects
import Signal exposing (Address)
import Debug

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

break_time: Int -> Int
break_time finished_block =
    if finished_block == 4 then 15 else 5

next_block: Int -> Int
next_block current_block =
    if current_block < 4 then
        current_block + 1
    else
        1

update: Action -> Model -> Model
update action model =
    case action of 
        Increment -> 
            if model.running then
                if model.seconds < 10 then
                    { model | seconds = model.seconds + 1 }
                else
                    if model.seconds < 10 then
                        { model | minutes = model.minutes + 1, seconds = 0 }
                    else
                        {
                            model |
                                minutes = 0,
                                seconds = 0,
                                current_block = next_block model.current_block,
                                running = False,
                                break_time_owed =
                                    model.break_time_owed + (break_time model.current_block)
                        }
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
