module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    {}


boardSlice : List ()
boardSlice =
    List.repeat 8 ()


board : List (List ())
board =
    boardSlice
        |> List.repeat 8


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model, Cmd.none )



-- Update


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view _ =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "50px 1fr"
        ]
        [ div
            [ style "display" "grid"
            , style "grid-template-columns" "50px"
            ]
          <|
            List.indexedMap (\i _ -> div [] [ text <| String.fromInt <| i + 1 ]) boardSlice
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(8, 50px)"
            , style "grid-template-rows" "repeat(8, 50px)"
            , style "width" "400px"
            , style "border" "1px solid black"
            ]
          <|
            (List.indexedMap
                (\i column ->
                    column
                        |> List.indexedMap
                            (\j _ ->
                                div
                                    [ style "background" <|
                                        if modBy 2 (i + j) == 0 then
                                            "brown"

                                        else
                                            "white"
                                    ]
                                    [ text <| String.fromInt i ++ String.fromInt j ]
                            )
                )
                board
                |> List.concat
            )
        , div
            [ style "display" "grid"
            , style "grid-template-columns" "repeat(8, 50px)"
            , style "grid-template-rows" "repeat(8, 50px)"
            , style "margin" "0 0 0 50px"
            ]
          <|
            List.indexedMap
                (\i _ ->
                    div []
                        [ text (i + 97 |> Char.fromCode >> String.fromChar) ]
                )
                boardSlice
        ]
