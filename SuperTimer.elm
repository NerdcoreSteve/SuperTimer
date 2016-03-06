import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import StartApp
import Effects
import Signal exposing (Address)
import Debug

type alias Clock =
    {
        hours : Int,
        minutes : Int,
        seconds : Int
    }

tick: Clock -> Clock
tick clock =
    if clock.seconds < 10 then
        Clock 0 clock.minutes (clock.seconds + 1)
    else
        Clock 0 (clock.minutes + 1) 0

type alias Model = 
    {
        work_clock: Clock,
        working : Bool,
        break_clock: Clock,
        breaking : Bool,
        current_block: Int
    }

type Action = Increment | StartPause | Reset | Break

init : Model 
init = Model (Clock 0 0 0) False (Clock 0 0 0) False 1

break_time: Int -> Int
break_time finished_block =
    if finished_block == 4 then 15 else 5

add_break_time: Clock -> Int -> Clock
add_break_time clock finished_block=
    if finished_block == 4 then
        {clock | minutes = clock.minutes + 15}
    else
        {clock | minutes = clock.minutes + 5}

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
            if model.working then
                if model.work_clock.seconds < 10 then
                    { model | work_clock = (tick model.work_clock)}
                else
                    Model
                        (Clock 0 0 0)
                        False
                        (add_break_time model.break_clock model.current_block)
                        False
                        (next_block model.current_block)
            else
                model
        StartPause -> 
            { model | working = not model.working }
        Reset -> 
            Model (Clock 0 0 0) False (Clock 0 0 0) False model.current_block
        Break -> 
            { model | breaking = not model.breaking }

format_double_digit: Int -> String
format_double_digit number =
    if number < 10 then "0" ++ (toString number) else toString number

clock_to_string clock =
    (format_double_digit clock.hours) ++
    ":" ++
    (format_double_digit clock.minutes) ++
    ":" ++
    (format_double_digit clock.seconds)

view : Address Action -> Model -> Html
view address model =
    div 
        []
        [
            div [] [text ("work clock: " ++ (clock_to_string model.work_clock))],
            div [] [text ("break clock: " ++ (clock_to_string model.break_clock))],
            div [] [text ("current block: " ++ (toString model.current_block))],
            div
                []
                [
                    button
                        [ onClick address StartPause ]
                        [ text <| if model.working then "Pause" else "Start" ],
                    button 
                        [onClick address Reset ]
                        [ text "Reset" ],
                    button 
                        [onClick address Break ]
                        [ text "Break" ]
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
