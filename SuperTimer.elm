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
        minutes_in_hour: Int,
        max_hours: Int
    }

type Direction = Forward | Backward

--TODO There's got to be a better way
tick: Clock -> Direction -> Clock
tick clock direction =
    readjust_clock <|
        case direction of 
            Forward ->
                case clock.time_balance of
                    Positive ->
                        {clock | seconds = clock.seconds + 1}
                    Negative ->
                        { clock | seconds = clock.seconds - 1 }
            Backward ->
                case clock.time_balance of
                    Positive ->
                        {clock | seconds = clock.seconds - 1}
                    Negative ->
                        { clock | seconds = clock.seconds + 1 }

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
        (Clock 0 0 0 Positive False 3 6 2)
        False
        (Clock 0 0 0 Positive True 3 6 2)
        False
        1
        5
        2
        4
        4

break_time: Int -> Int
break_time finished_block =
    if finished_block == 4 then 15 else 5

--TODO super repetative
readjust_clock: Clock -> Clock
readjust_clock clock =
    if clock.seconds > clock.seconds_in_minute then
        readjust_clock
            { clock |
                minutes =
                    clock.minutes + (clock.seconds // clock.seconds_in_minute),
                seconds = clock.seconds % clock.seconds_in_minute - 1 }
    else if clock.minutes > clock.minutes_in_hour then
        readjust_clock
            { clock |
                hours =
                    clock.hours + (clock.minutes // clock.minutes_in_hour),
                minutes = clock.minutes % clock.minutes_in_hour - 1 }
    else if clock.hours > clock.max_hours then
        readjust_clock
            { clock |
                hours = clock.hours % clock.max_hours - 1 }
    else if clock.seconds < 0 then
        readjust_clock
            { clock |
                minutes = clock.minutes - 1,
                seconds = clock.seconds_in_minute - 1 }
    else if clock.minutes < 0 then
        readjust_clock
            { clock |
                hours = clock.hours - 1,
                minutes = clock.minutes_in_hour - 1 }
    else if clock.hours < 0 then
            { clock |
                hours = 0,
                minutes = 0,
                seconds = 1,
                time_balance = Negative }
    else
        clock

add_break_time: Clock -> Int -> Clock
add_break_time clock finished_block=
    readjust_clock <|
        let
            break_time = if finished_block == 4 then 15 else 5
        in
            case clock.time_balance of
                Positive ->
                    {clock | minutes = clock.minutes + break_time}
                Negative ->
                    {clock | minutes = clock.minutes - break_time}

next_block: Model -> Int
next_block model =
    if model.current_block < model.number_of_blocks_till_longer_break then
        model.current_block + 1
    else
        1

--TODO is there duplicate work with readjust_clock here?
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
                    { model | break_clock = (tick model.break_clock Backward)}
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
