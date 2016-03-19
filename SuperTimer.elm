import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Time exposing (every, second)
import StartApp
import Effects
import Signal exposing (Address)
import Debug

type TimeBalance = Negative | Positive

type alias Clock =
    {
        hours : Int,
        minutes : Int,
        seconds : Int,
        time_balance : TimeBalance,
        show_balance : Bool,
        seconds_in_minute: Int,
        minutes_in_hours: Int
    }


type Direction = Forward | Backward

tick: Clock -> Direction -> Clock
tick clock direction =
    case direction of 
        Forward ->
            if clock.seconds < clock.seconds_in_minute then
                { clock | hours = 0, seconds = clock.seconds + 1 }
            else
                { clock | hours = 0, minutes = clock.minutes + 1, seconds = 0 }
        Backward ->
            if clock.seconds > 0 then
                {clock | seconds = clock.seconds - 1}
            else if clock.minutes > 0 then
                    {clock | minutes = clock.minutes - 1, seconds = clock.seconds_in_minute}
            else
                clock

type alias Model = 
    {
        work_clock: Clock,
        working : Bool,
        break_clock: Clock,
        breaking : Bool,
        current_block: Int,
        minutes_in_work_block: Int,
        minutes_in_break_block: Int,
        minutes_in_longer_break_block: Int,
        number_of_blocks_till_longer_break: Int
    }

type Action = Increment | Work | Break

--Magic numbers, gotta be a better way
init : Model 
init =
    Model
        (Clock 0 0 0 Positive False 3 6)
        False
        (Clock 0 0 0 Positive True 3 6)
        False
        1
        5
        2
        4
        4

break_time: Int -> Int
break_time finished_block =
    if finished_block == 4 then 15 else 5

add_break_time: Clock -> Int -> Clock
add_break_time clock finished_block=
    if finished_block == 4 then
        {clock | minutes = clock.minutes + 15}
    else
        {clock | minutes = clock.minutes + 5}

next_block: Model -> Int
next_block model =
    if model.current_block < model.number_of_blocks_till_longer_break then
        model.current_block + 1
    else
        1

update: Action -> Model -> Model
update action model =
    case action of 
        Increment -> 
            let old_clock = model.work_clock in
                if model.working then
                    if model.work_clock.minutes < model.minutes_in_work_block then
                        { model | work_clock = (tick model.work_clock Forward)}
                    else
                        { model | 
                            work_clock =
                                { old_clock | hours = 0, minutes = 0, seconds = 0 },
                            break_clock = add_break_time model.break_clock model.current_block,
                            breaking = False,
                            working = False,
                            current_block = next_block model
                        }
                else if model.breaking then
                    if model.break_clock.minutes > 0 || model.break_clock.seconds > 0 then
                        { model | break_clock = (tick model.break_clock Backward)}
                    else
                        { model | breaking = False }
                else
                    model
        Work -> 
            { model | working = not model.working, breaking = False }
        Break -> 
            { model | breaking = not model.breaking, working = False }

format_double_digit: Int -> String
format_double_digit number =
    if number < 10 then "0" ++ (toString number) else toString number

clock_display_balance: Clock -> String
clock_display_balance clock =
    if clock.show_balance then
        case clock.time_balance of
            Negative ->
                "- "
            Positive ->
                "+ "
    else
        ""

clock_display: Clock  -> String
clock_display clock =
    (clock_display_balance clock) ++
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
            div [] [text ("work clock: " ++ (clock_display model.work_clock))],
            div [] [text ("break clock: " ++ (clock_display model.break_clock))],
            div [] [text ("current block: " ++ (toString model.current_block))],
            div
                []
                [
                    button
                        [ onClick address Work ]
                        [ text <| if model.working then "Pause Work" else "Start Work" ],
                    button 
                        [onClick address Break ]
                        [ text <| if model.breaking then "Pause Break" else "Start Break" ]
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
