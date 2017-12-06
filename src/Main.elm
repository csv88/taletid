module Main exposing (..)

import Html exposing (Html, text, div, h1, img, button, p)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Time.DateTime as DateTime exposing (DateTime, DateTimeDelta, hour, minute, second)
import Time exposing (Time, every)
import Task


---- MODEL ----


type alias Model =
    { gjeldendeTid : DateTime
    , starttid : DateTime
    , maltid : DateTime
    , nedtelling : DateTimeDelta
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gjeldendeTid = DateTime.epoch
      , starttid = DateTime.epoch
      , maltid = DateTime.epoch
      , nedtelling = deltaZero
      }
    , getTime
    )


deltaZero : DateTimeDelta
deltaZero =
    DateTime.delta DateTime.epoch DateTime.epoch



---- UPDATE ----


type Msg
    = OnTime Time
    | SettMaltid Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTime t ->
            let
                gjeldende =
                    DateTime.fromTimestamp t

                opptalt =
                    DateTime.delta model.maltid model.gjeldendeTid

                x =
                    if opptalt.seconds < 0 then
                        deltaZero
                    else
                        opptalt
            in
                ( { model | gjeldendeTid = gjeldende, nedtelling = x }, Cmd.none )

        SettMaltid minutter ->
            let
                nyTid =
                    DateTime.addMinutes minutter model.gjeldendeTid
            in
                ( { model | starttid = model.gjeldendeTid, maltid = nyTid }, getTime )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Taletid" ]
          --, p [] [ text <| formaterTidspunkt <| model.gjeldendeTid ]
          --, p [] [ text <| formaterTidspunkt <| model.maltid ]
          --, viewProgressbar 100
        , p [] [ text <| formaterTidspunkt2 <| model.nedtelling ]
        , visKnapp 1
        , visKnapp 3
        , visKnapp 5
        , visKnapp 7
        , visKnapp 10
        , button
            [ onClick <| SettMaltid 0 ]
            [ text "Nullstill" ]
        ]


visKnapp : Int -> Html Msg
visKnapp antallMin =
    button
        [ onClick <| SettMaltid antallMin ]
        [ text <| toString antallMin ++ " min" ]


viewProgressbar : Int -> Html Msg
viewProgressbar int =
    div [ class "progressbar" ]
        [ div
            [ class "progress"
            , style [ ( "width", (toString int) ++ "%" ) ]
            ]
            []
        ]


formaterTidspunkt : DateTime -> String
formaterTidspunkt gjeldendeTidspunkt =
    let
        lagTid tidstype =
            gjeldendeTidspunkt
                |> tidstype
                |> toString
                |> String.padLeft 2 '0'
    in
        lagTid hour
            ++ ":"
            ++ lagTid minute
            ++ ":"
            ++ lagTid second


formaterTidspunkt2 : DateTimeDelta -> String
formaterTidspunkt2 gjeldendeTidspunkt =
    String.padLeft 2 '0' (toString (gjeldendeTidspunkt.minutes))
        ++ ":"
        ++ String.padLeft 2 '0' (toString (gjeldendeTidspunkt.seconds - gjeldendeTidspunkt.minutes * 60))



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    every Time.second OnTime



---- PROGRAM ----


type alias Flags =
    {}


getTime : Cmd Msg
getTime =
    Time.now
        |> Task.perform OnTime


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
