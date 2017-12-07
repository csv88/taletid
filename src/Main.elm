module Main exposing (..)

import Html exposing (Attribute, Html, button, div, h1, img, p, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (keyCode, on, onClick)
import Task
import Time exposing (Time, every)
import Time.DateTime as DateTime exposing (DateTime, DateTimeDelta, hour, minute, second)
import Keyboard


---- MODEL ----


type alias Model =
    { gjeldendeTid : DateTime
    , startTid : DateTime
    , sluttTid : DateTime
    , nedtelling : DateTimeDelta
    , prosentFerdig : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { gjeldendeTid = DateTime.epoch
      , startTid = DateTime.epoch
      , sluttTid = DateTime.epoch
      , nedtelling = deltaZero
      , prosentFerdig = 0
      }
    , getTime
    )


deltaZero : DateTimeDelta
deltaZero =
    DateTime.delta DateTime.epoch DateTime.epoch


prosentFerdig : Model -> Float
prosentFerdig model =
    let
        gjeldende =
            DateTime.delta model.gjeldendeTid model.startTid

        totalt =
            DateTime.delta model.sluttTid model.startTid

        prosent =
            (toFloat gjeldende.milliseconds) / (toFloat totalt.milliseconds) * 100
    in
        prosent



---- UPDATE ----


type Msg
    = OnTime Time
    | SettMaltid Int
    | Keypress Keyboard.KeyCode


type Key
    = KeyEnter
    | KeyA


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTime t ->
            let
                b =
                    x model t
            in
                ( b
                , Cmd.none
                )

        Keypress key ->
            let
                nyTid minutter =
                    DateTime.addMinutes minutter model.gjeldendeTid

                _ =
                    Debug.log "Keypress!" key

                tid =
                    case key of
                        13 ->
                            nyTid 20

                        _ ->
                            nyTid 30
            in
                ( { model | startTid = model.gjeldendeTid, sluttTid = tid }, getTime )

        SettMaltid minutter ->
            let
                nyTid =
                    DateTime.addMinutes minutter model.gjeldendeTid
            in
                ( { model | startTid = model.gjeldendeTid, sluttTid = nyTid }, getTime )


x : Model -> Time -> Model
x model t =
    let
        gjeldende =
            DateTime.fromTimestamp t

        opptalt =
            DateTime.delta model.sluttTid model.gjeldendeTid

        dd =
            if opptalt.seconds < 0 then
                deltaZero
            else
                opptalt

        prosent_ =
            prosentFerdig model

        prosent =
            if prosent_ < 100 then
                prosent_
            else
                100
    in
        { model
            | gjeldendeTid = gjeldende
            , nedtelling = dd
            , prosentFerdig = prosent
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Taletid" ]
        , p [] [ text <| formaterTidspunkt2 <| model.nedtelling ]
        , viewProgressbar model.prosentFerdig
        , visKnapp 1
        , visKnapp 3
        , visKnapp 5
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


viewProgressbar : Float -> Html Msg
viewProgressbar prosent =
    div [ class "progressbar" ]
        [ div
            [ class "progress"
            , style [ ( "width", (toString prosent) ++ "%" ) ]
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs Keypress
        , every Time.second OnTime
        ]



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
