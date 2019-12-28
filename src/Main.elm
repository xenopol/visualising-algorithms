module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, input, span, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onInput)
import KnightBFS exposing (Move, algebraicToMove, charACode, getKnightMoves, moveToAlgebraic, validateMove)



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
    { boardLength : Int
    , board : Board
    , startPos : Move
    , startPosInputValue : String
    , endPos : Move
    , endPosInputValue : String
    , error : Maybe String
    }


board_length : Int
board_length =
    8


type alias Board =
    List BoardSlice


type alias BoardSlice =
    List ()


startPos : Move
startPos =
    ( 6, 2, 0 )


endPos : Move
endPos =
    ( 2, 1, 0 )


initialModel : Model
initialModel =
    { boardLength = board_length
    , board = getBoard board_length
    , startPos = startPos
    , startPosInputValue = moveToAlgebraic startPos
    , endPos = endPos
    , endPosInputValue = moveToAlgebraic endPos
    , error = Nothing
    }


getBoardSlice : Int -> BoardSlice
getBoardSlice boardLength =
    List.repeat boardLength ()


getBoard : Int -> Board
getBoard boardLength =
    getBoardSlice boardLength
        |> List.repeat boardLength


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )



-- Update


type Msg
    = UpdateStartPos String
    | UpdateEndPos String
    | UpdateBoardLength String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStartPos value ->
            case algebraicToMove model.boardLength value of
                Ok pos ->
                    ( { model
                        | startPos = pos
                        , startPosInputValue = value
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | startPosInputValue = value
                        , error = Just error
                      }
                    , Cmd.none
                    )

        UpdateEndPos value ->
            case algebraicToMove model.boardLength value of
                Ok pos ->
                    ( { model
                        | endPos = pos
                        , endPosInputValue = value
                        , error = Nothing
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | endPosInputValue = value
                        , error = Just error
                      }
                    , Cmd.none
                    )

        UpdateBoardLength value ->
            let
                boardLength =
                    String.toInt value |> Maybe.withDefault board_length

                newModel =
                    if boardLength >= board_length then
                        { model
                            | boardLength = boardLength
                            , board = getBoard boardLength
                        }

                    else
                        model
            in
            ( newModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Visualizing Breadth First Search - Knight shortest path" ]
        , h1 []
            [ span [] [ text "Number of moves: " ]
            , span []
                [ text <|
                    String.fromInt <|
                        Result.withDefault 0 <|
                            getKnightMoves model.startPosInputValue model.endPosInputValue model.boardLength
                ]
            ]
        , div [ class "config-container" ]
            [ div []
                [ span [] [ text "Board length" ]
                , input
                    [ onInput UpdateBoardLength
                    , type_ "number"
                    , value <| String.fromInt model.boardLength
                    ]
                    []
                ]
            , div []
                [ span [] [ text "Start position" ]
                , input
                    [ onInput UpdateStartPos
                    , value model.startPosInputValue
                    , placeholder "e.g. a1"
                    ]
                    []
                ]
            , div []
                [ span [] [ text "End position" ]
                , input
                    [ onInput UpdateEndPos
                    , value model.endPosInputValue
                    , placeholder "e.g. b3"
                    ]
                    []
                ]
            ]
        , div
            [ class "board-container"
            , style "display" "grid"
            , style "grid-template-columns" "repeat(9, 50px)"
            , style "grid-template-rows" "repeat(9, 50px)"
            , style
                "grid-template-areas"
                (getGridTemplateAreas model.boardLength)
            ]
            [ div
                [ class "rows-label"
                , style "display" "grid"
                , style "grid-template-columns" "50px"
                , style "grid-template-rows" <| "repeat(" ++ String.fromInt model.boardLength ++ ", 50px)"
                , style "grid-area" "row"
                ]
              <|
                List.indexedMap
                    (\i _ -> div [] [ text <| String.fromInt <| i + 1 ])
                    (getBoardSlice model.boardLength)
            , div
                [ class "columns-label"
                , style "display" "grid"
                , style "grid-template-columns" <| "repeat(" ++ String.fromInt model.boardLength ++ ", 50px)"
                , style "grid-template-rows" "50px"
                , style "grid-area" "column"
                ]
              <|
                List.indexedMap
                    (\i _ ->
                        div []
                            [ text (i + charACode |> Char.fromCode >> String.fromChar) ]
                    )
                    (getBoardSlice model.boardLength)
            , div
                [ style "display" "grid"
                , style "grid-template-columns" <| "repeat(" ++ String.fromInt model.boardLength ++ ", 50px)"
                , style "grid-template-rows" <| "repeat(" ++ String.fromInt model.boardLength ++ ", 50px)"
                , style "grid-area" "board"
                , style "width" <| String.fromInt (model.boardLength * 50) ++ "px"
                , style "outline" "1px solid black"
                ]
              <|
                (List.indexedMap
                    (\i column ->
                        column
                            |> List.indexedMap
                                (\j _ ->
                                    div
                                        [ style "background" <|
                                            if ( i, j, 0 ) == model.startPos then
                                                "blue"

                                            else if ( i, j, 0 ) == model.endPos then
                                                "green"

                                            else if modBy 2 (i + j) == 0 then
                                                "#6d4c41"

                                            else
                                                "#e0e0e0"
                                        ]
                                        [ text <| String.fromInt i ++ String.fromInt j ]
                                )
                    )
                    model.board
                    |> List.concat
                )
            ]
        ]


getGridTemplateAreas : Int -> String
getGridTemplateAreas boardLength =
    List.repeat boardLength ""
        |> List.map
            (\_ ->
                "\"row " ++ String.repeat boardLength "board " ++ "\""
            )
        |> (++) [ "\". " ++ String.repeat boardLength "column " ++ "\"" ]
        |> String.join "\n"
