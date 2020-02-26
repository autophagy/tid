module Main exposing (..)

import Browser
import Html exposing (Html, a, button, div, h1, img, input, text)
import Html.Attributes exposing (class, classList, id, maxlength, name, src, title, type_, value)
import Html.Events exposing (onClick, onInput)



---- MODEL ----


type alias Timer =
    { id : Int
    , title : String
    , seconds : Int
    , playing : Bool
    }


type alias Time =
    { hours : Int
    , minutes : Int
    , seconds : Int
    }


type alias Model =
    { newTimer : Timer
    , timers : List Timer
    }


initTimer : Timer
initTimer =
    { id = 0, title = "New Timer", seconds = 0, playing = False }


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


updateTimer : Int -> String -> Timer -> Timer
updateTimer id newTitle timer =
    if timer.id == id then
        { timer | title = newTitle }

    else
        timer



---- UPDATE ----


type Msg
    = AddTimer
    | DeleteLastTimer
    | PlayTimers
    | PauseTimers
    | TimerTitleChange Int String
    | ToggleTimer Int
    | DeleteTimer Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTimer ->
            let
                newTimer =
                    model.newTimer

                newTimerTemplate =
                    { newTimer | id = newTimer.id + 1 }
            in
            ( { model | timers = newTimer :: model.timers, newTimer = newTimerTemplate }, Cmd.none )

        DeleteLastTimer ->
            ( { model | timers = Maybe.withDefault [] (List.tail model.timers) }, Cmd.none )

        PlayTimers ->
            ( { model | timers = List.map (\timer -> { timer | playing = True }) model.timers }, Cmd.none )

        PauseTimers ->
            ( { model | timers = List.map (\timer -> { timer | playing = False }) model.timers }, Cmd.none )

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

        ToggleTimer timerId ->
            ( { model
                | timers =
                    List.map
                        (\timer ->
                            if timer.id == timerId then
                                { timer | playing = not timer.playing }

                            else
                                timer
                        )
                        model.timers
              }
            , Cmd.none
            )

        DeleteTimer timerId ->
            ( { model | timers = List.filter (\timer -> timer.id /= timerId) model.timers }, Cmd.none )



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
            (List.map viewTimer (List.reverse model.timers))
        ]


viewTimer : Timer -> Html Msg
viewTimer timer =
    div
        [ id (String.fromInt timer.id), class "tid" ]
        [ div [ class "tid-id" ] [ text (String.fromInt timer.id) ]
        , div [ class "tid-time" ]
            [ input
                [ type_ "text"
                , name "hours"
                , maxlength 2
                , value (formatTime (totalSecondsToHour timer.seconds))
                ]
                []
            , text "."
            , input
                [ type_ "text"
                , name "hours"
                , maxlength 2
                , value (formatTime (totalSecondsToMinutes timer.seconds))
                ]
                []
            , text "."
            , input
                [ type_ "text"
                , name "hours"
                , maxlength 2
                , value (formatTime (totalSecondsToSeconds timer.seconds))
                ]
                []
            ]
        , div [ class "tid-title" ]
            [ input
                [ type_ "text"
                , name "timer-title"
                , maxlength 20
                , value timer.title
                , onInput (TimerTitleChange timer.id)
                ]
                []
            ]
        , div [ class "tid-control" ]
            [ button
                [ title "Start/Pause the timer"
                , classList
                    [ ( "control-item", True )
                    , ( "play-icon", not timer.playing )
                    , ( "pause-icon", timer.playing )
                    ]
                , onClick (ToggleTimer timer.id)
                ]
                []
            , button
                [ title "Close the timer"
                , classList
                    [ ( "control-item", True )
                    , ( "close-icon", True )
                    ]
                , onClick (DeleteTimer timer.id)
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
        , subscriptions = always Sub.none
        }
