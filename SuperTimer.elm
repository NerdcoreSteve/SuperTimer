import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import StartApp
import Effects
import Signal exposing (Address)
import Debug

type alias Clock =
    {
        minutes : Int,
        seconds : Int
    }

tick: Clock -> Clock
tick clock =
    if clock.seconds < 10 then
        Clock clock.minutes (clock.seconds + 1)
    else
        Clock (clock.minutes + 1) 0

type alias Model = 
    {
        work_clock: Clock,
        break_clock: Clock,
        running : Bool,
        current_block: Int,
        break_time_owed: Int
    }

type Action = Increment | StartPause | Reset

init : Model 
init = Model (Clock 0 0) (Clock 0 0) False 1 0

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
                if model.work_clock.seconds < 10 then
                    { model | work_clock = (tick model.work_clock)}
                else
                    Model
                        (Clock 0 0)
                        (Clock 0 0)
                        False
                        (next_block model.current_block)
                        (model.break_time_owed + (break_time model.current_block))
            else
                model
        StartPause -> 
            { model | running = not model.running }
        Reset -> 
            Model (Clock 0 0) (Clock 0 0) False model.current_block model.break_time_owed

clock_to_string clock =
    (toString clock.minutes) ++
    ":" ++
    (toString clock.seconds)

view : Address Action -> Model -> Html
view address model =
    div 
        []
        [
            div [] [text ("work clock: " ++ (clock_to_string model.work_clock))],
            div [] [text ("break clock: " ++ (clock_to_string model.break_clock))],
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
