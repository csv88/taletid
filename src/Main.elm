module Main exposing (..)

import Char
import Html exposing (Attribute, Html, button, div, h1, img, p, span, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (keyCode, on, onClick)
import Keyboard exposing (KeyCode)
import List
import Time exposing (Time, every, second)
import Time.DateTime as DateTime exposing (DateTime, DateTimeDelta, hour, minute)


---- MODEL ----


type alias Model =
    { gjeldendeTid : DateTime
    , startTid : DateTime
    , sluttTid : DateTime
    , nedtelling : DateTimeDelta
    , prosentFerdig : Float
    , valg : List Valg
    , varsel : Bool
    }


type alias Valg =
    { minutter : Int
    , hurtigtast : KeyCode
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    { gjeldendeTid = DateTime.epoch
    , startTid = DateTime.epoch
    , sluttTid = DateTime.epoch
    , nedtelling = deltaZero
    , prosentFerdig = 0
    , valg =
        [ Valg 1 (Char.toCode 'R')
        , Valg 2 (Char.toCode 'D')
        , Valg 3 (Char.toCode 'I')
        , Valg 5 (Char.toCode 'H')
        , Valg 10 (Char.toCode 'S')
        , Valg 60 (Char.toCode 'P')
        ]
    , varsel = False
    }
        ! []


deltaZero : DateTimeDelta
deltaZero =
    DateTime.delta DateTime.epoch DateTime.epoch


beregnProsentFerdig : DateTime -> DateTime -> DateTime -> Float
beregnProsentFerdig gjeldendeTid startTid sluttTid =
    let
        gjeldende =
            DateTime.delta gjeldendeTid startTid

        totalt =
            DateTime.delta sluttTid startTid

        prosent_ =
            (toFloat gjeldende.milliseconds) / (toFloat totalt.milliseconds) * 100

        prosent =
            if prosent_ < 100 then
                prosent_
            else
                100
    in
        prosent



---- UPDATE ----


type Msg
    = OnTime Time
    | SettMaltid Int
    | Keypress KeyCode
    | Setvarsel Bool


type Key
    = KeyEnter
    | KeyA


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Setvarsel v ->
            { model | varsel = v } ! []

        OnTime t ->
            let
                b =
                    x model t

                cc =
                    if model.nedtelling.seconds == 0 then
                        update (Setvarsel True)
                    else if model.nedtelling.seconds < 31 then
                        update (Setvarsel True)
                    else
                        update (Setvarsel False)
            in
                b |> cc

        Keypress key ->
            let
                _ =
                    Debug.log "Keypress!" key

                tid =
                    model.valg
                        |> List.filter (\x -> x.hurtigtast == key)
                        |> List.map (\x -> x.minutter)
                        |> List.head
                        |> Maybe.withDefault 0
                        |> settNySluttTid model.gjeldendeTid
            in
                { model | startTid = model.gjeldendeTid, sluttTid = tid } ! []

        SettMaltid minutter ->
            let
                nySluttTid =
                    settNySluttTid model.gjeldendeTid minutter
            in
                { model | startTid = model.gjeldendeTid, sluttTid = nySluttTid } ! []


settNySluttTid : DateTime -> Int -> DateTime
settNySluttTid gjeldendeTid minutter =
    DateTime.addMinutes minutter gjeldendeTid


x : Model -> Time -> Model
x model t =
    let
        gjeldende =
            DateTime.fromTimestamp t

        opptalt =
            DateTime.delta model.sluttTid model.gjeldendeTid

        nedtelling =
            if opptalt.seconds < 0 then
                deltaZero
            else
                opptalt

        prosent =
            beregnProsentFerdig model.gjeldendeTid model.startTid model.sluttTid
    in
        { model
            | gjeldendeTid = gjeldende
            , nedtelling = nedtelling
            , prosentFerdig = prosent
        }



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ if model.varsel == True then
            class "varsel"
          else
            class ""
        ]
    <|
        List.concat
            [ [ h1 [] [ text "" ]
              , p
                    []
                    [ text <| formaterTidspunkt2 <| model.nedtelling ]
              , viewProgressbar model
              ]
            , List.map viewKnapp model.valg
            , [ button
                    [ onClick <| SettMaltid 0 ]
                    [ text "Nullstill" ]
              ]
            ]


viewKnapp : Valg -> Html Msg
viewKnapp valg =
    button
        [ onClick <| SettMaltid valg.minutter ]
        [ text <| toString valg.minutter ++ " min" ]


viewProgressbar : Model -> Html Msg
viewProgressbar model =
    div [ class "progressbar" ]
        [ div
            [ if model.varsel == True then
                class "progressvarsel"
              else
                class "progress"
            , style [ ( "width", (toString model.prosentFerdig) ++ "%" ) ]
            ]
            []
        ]


formaterTidspunkt2 : DateTimeDelta -> String
formaterTidspunkt2 gjeldendeTidspunkt =
    let
        padd str =
            String.padLeft 2 '0' (toString (str))
    in
        padd gjeldendeTidspunkt.minutes
            ++ ":"
            ++ padd (gjeldendeTidspunkt.seconds - gjeldendeTidspunkt.minutes * 60)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs Keypress
        , every (0.1 * second) OnTime
        ]



---- PROGRAM ----


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
