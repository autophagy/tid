port module Main exposing (Model, Msg, Time, Timer, main)

import Browser
import Html exposing (Html, a, button, div, img, input, text)
import Html.Attributes exposing (class, classList, id, maxlength, name, readonly, src, title, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time



---- MODEL ----


type alias Timer =
    { id : Int
    , title : String
    , timeLeft : Time
    , targetTime : Maybe Time.Posix
    }


type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


setHours : String -> Time -> Time
setHours text timeLeft =
    let
        newHour : Int
        newHour =
            Maybe.withDefault 0 (String.toInt text)
    in
    { timeLeft | hours = newHour }


setMinutes : String -> Time -> Time
setMinutes text timeLeft =
    let
        newMinute : Int
        newMinute =
            text
                |> String.toInt
                |> Maybe.withDefault 0
                |> clamp 0 59
    in
    { timeLeft | minutes = newMinute }


setSeconds : String -> Time -> Time
setSeconds text timeLeft =
    let
        newSecond : Int
        newSecond =
            text
                |> String.toInt
                |> Maybe.withDefault 0
                |> clamp 0 59
    in
    { timeLeft | seconds = newSecond }


type alias Model =
    { newTimer : Timer
    , timers : List Timer
    }


initTimer : Timer
initTimer =
    { id = 0, title = "New Timer", timeLeft = { hours = 0, minutes = 0, seconds = 0 }, targetTime = Nothing }


init : ( Model, Cmd Msg )
init =
    ( { newTimer = initTimer, timers = [] }, Cmd.none )


totalSecondsToHour : Int -> Int
totalSecondsToHour seconds =
    seconds // 3600


totalSecondsToMinutes : Int -> Int
totalSecondsToMinutes seconds =
    (seconds - (totalSecondsToHour seconds * 3600)) // 60


totalSecondsToSeconds : Int -> Int
totalSecondsToSeconds seconds =
    seconds - (totalSecondsToHour seconds * 3600) - (totalSecondsToMinutes seconds * 60)


formatTime : Int -> String
formatTime time =
    if time < 10 then
        "0" ++ String.fromInt time

    else
        String.fromInt time


totalSeconds : Time -> Int
totalSeconds timeLeft =
    (timeLeft.hours * 3600) + (timeLeft.minutes * 60) + timeLeft.seconds


refreshTimeLeft : Time.Posix -> Time.Posix -> Time
refreshTimeLeft target now =
    let
        diff : Int
        diff =
            (Time.posixToMillis target - Time.posixToMillis now) // 1000
    in
    { hours = totalSecondsToHour diff, minutes = totalSecondsToMinutes diff, seconds = totalSecondsToSeconds diff }


lowestPlayingTime : List Timer -> Maybe Int
lowestPlayingTime timers =
    List.head <| List.sort <| List.map (\timer -> totalSeconds timer.timeLeft) <| List.filter timerIsPlaying timers


formatTotalSeconds : Int -> String
formatTotalSeconds seconds =
    "[" ++ (formatTime <| totalSecondsToHour seconds) ++ ":" ++ (formatTime <| totalSecondsToMinutes seconds) ++ ":" ++ (formatTime <| totalSecondsToSeconds seconds) ++ "]"


timerIsPlaying : Timer -> Bool
timerIsPlaying timer =
    case timer.targetTime of
        Just _ ->
            True

        Nothing ->
            False


createTargetTime : Time.Posix -> Time -> Time.Posix
createTargetTime started delta =
    Time.millisToPosix (Time.posixToMillis started + (totalSeconds delta * 1000))



---- UPDATE ----


type Msg
    = AddTimer
    | DeleteLastTimer
    | PlayTimers
    | PauseTimers
    | TimerTitleChange Int String
    | TimerTimeChange (String -> Time -> Time) Int String
    | PlayTimer Int
    | SetTimerTarget Int Time.Posix
    | StopTimer Int
    | DeleteTimer Int
    | RefreshTimeLeftTick Time.Posix
    | AlertTick Time.Posix
    | UpdatePageTitleTick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTimer ->
            let
                newTimer : Timer
                newTimer =
                    model.newTimer

                newTimerTemplate : Timer
                newTimerTemplate =
                    { newTimer | id = newTimer.id + 1 }
            in
            ( { model | timers = newTimer :: model.timers, newTimer = newTimerTemplate }, Cmd.none )

        DeleteLastTimer ->
            ( { model | timers = Maybe.withDefault [] (List.tail model.timers) }, Cmd.none )

        PlayTimers ->
            ( model, Cmd.batch (List.map (\timer -> Task.perform (SetTimerTarget timer.id) Time.now) model.timers) )

        PauseTimers ->
            ( { model | timers = List.map (\timer -> { timer | targetTime = Nothing }) model.timers }, Cmd.none )

        TimerTitleChange timerId text ->
            ( { model
                | timers =
                    List.map
                        (\timer ->
                            if timer.id == timerId then
                                { timer | title = text }

                            else
                                timer
                        )
                        model.timers
              }
            , Cmd.none
            )

        TimerTimeChange f timerId text ->
            ( { model
                | timers =
                    List.map
                        (\timer ->
                            if timer.id == timerId then
                                { timer | timeLeft = f text timer.timeLeft }

                            else
                                timer
                        )
                        model.timers
              }
            , Cmd.none
            )

        PlayTimer timerId ->
            ( model, Task.perform (SetTimerTarget timerId) Time.now )

        SetTimerTarget timerId time ->
            ( { model
                | timers =
                    List.map
                        (\timer ->
                            if timer.id == timerId then
                                case timer.targetTime of
                                    Just _ ->
                                        timer

                                    Nothing ->
                                        { timer | targetTime = Just <| createTargetTime time timer.timeLeft }

                            else
                                timer
                        )
                        model.timers
              }
            , Cmd.none
            )

        StopTimer timerId ->
            ( { model
                | timers =
                    List.map
                        (\timer ->
                            if timer.id == timerId then
                                { timer | targetTime = Nothing }

                            else
                                timer
                        )
                        model.timers
              }
            , Cmd.none
            )

        DeleteTimer timerId ->
            ( { model | timers = List.filter (\timer -> timer.id /= timerId) model.timers }, Cmd.none )

        RefreshTimeLeftTick time ->
            ( { model
                | timers =
                    List.map
                        (\timer ->
                            case timer.targetTime of
                                Just targetTime ->
                                    if totalSeconds timer.timeLeft > 0 then
                                        { timer | timeLeft = refreshTimeLeft targetTime time }

                                    else
                                        timer

                                Nothing ->
                                    timer
                        )
                        model.timers
              }
            , Cmd.none
            )

        AlertTick _ ->
            let
                shouldPlay : Bool
                shouldPlay =
                    List.length (List.filter (\timer -> timerIsPlaying timer && totalSeconds timer.timeLeft == 0) model.timers) > 0
            in
            ( model, playAlert shouldPlay )

        UpdatePageTitleTick _ ->
            case lowestPlayingTime model.timers of
                Just t ->
                    ( model, updatePageTitle <| formatTotalSeconds t ++ " Tid" )

                Nothing ->
                    ( model, updatePageTitle "Tid" )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ Time.every 1000 RefreshTimeLeftTick, Time.every 1000 AlertTick, Time.every 1000 UpdatePageTitleTick ]



---- PORTS ----


port playAlert : Bool -> Cmd msg


port updatePageTitle : String -> Cmd msg



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ id "wrapper" ]
        [ div [ id "sidebar" ]
            [ img [ src "/tid.png", id "logo" ] []
            , div [ class "control-panel" ]
                [ a [ onClick AddTimer ] [ text "add" ]
                , a [ onClick DeleteLastTimer ] [ text "delete" ]
                , a [ onClick PlayTimers ] [ text "play" ]
                , a [ onClick PauseTimers ] [ text "pause" ]
                ]
            ]
        , div [ id "tidspace" ]
            (model.timers
                |> List.reverse
                |> List.map viewTimer
            )
        ]


viewTimer : Timer -> Html Msg
viewTimer timer =
    div
        [ id (String.fromInt timer.id)
        , classList
            [ ( "tid", True )
            , ( "playing", timerIsPlaying timer && totalSeconds timer.timeLeft > 0 )
            , ( "finished", timerIsPlaying timer && totalSeconds timer.timeLeft == 0 )
            ]
        ]
        [ div [ class "tid-id" ] [ String.fromInt timer.id |> text ]
        , div [ class "tid-time" ]
            [ input
                [ type_ "text"
                , name "hours"
                , maxlength 2
                , value
                    (if timerIsPlaying timer then
                        formatTime timer.timeLeft.hours

                     else
                        String.fromInt timer.timeLeft.hours
                    )
                , readonly <| timerIsPlaying timer
                , TimerTimeChange setHours timer.id |> onInput
                ]
                []
            , text "."
            , input
                [ type_ "text"
                , name "minutes"
                , maxlength 2
                , value
                    (if timerIsPlaying timer then
                        formatTime timer.timeLeft.minutes

                     else
                        String.fromInt timer.timeLeft.minutes
                    )
                , readonly <| timerIsPlaying timer
                , TimerTimeChange setMinutes timer.id |> onInput
                ]
                []
            , text "."
            , input
                [ type_ "text"
                , name "seconds"
                , maxlength 2
                , value
                    (if timerIsPlaying timer then
                        formatTime timer.timeLeft.seconds

                     else
                        String.fromInt timer.timeLeft.seconds
                    )
                , readonly <| timerIsPlaying timer
                , TimerTimeChange setSeconds timer.id |> onInput
                ]
                []
            ]
        , div [ class "tid-title" ]
            [ input
                [ type_ "text"
                , name "timer-title"
                , maxlength 20
                , value timer.title
                , TimerTitleChange timer.id |> onInput
                ]
                []
            ]
        , div [ class "tid-control" ]
            [ button
                [ title "Start/Pause the timer"
                , classList
                    [ ( "control-item", True )
                    , ( "play-icon", not <| timerIsPlaying timer )
                    , ( "pause-icon", timerIsPlaying timer )
                    ]
                , if timerIsPlaying timer then
                    StopTimer timer.id |> onClick

                  else
                    PlayTimer timer.id |> onClick
                ]
                []
            , button
                [ title "Close the timer"
                , classList
                    [ ( "control-item", True )
                    , ( "close-icon", True )
                    ]
                , DeleteTimer timer.id |> onClick
                ]
                []
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
