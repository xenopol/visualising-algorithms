module Main exposing (Model, Msg, init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, button, div, h1, input, span, text)
import Html.Attributes exposing (class, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import KnightBFS as K



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
    { board : Board
    , startPosInputValue : String
    , endPosInputValue : String
    , knightBFSModel : K.Model
    , error : Maybe String
    }


board_length : Int
board_length =
    8


type alias Board =
    List BoardSlice


type alias BoardSlice =
    List ()


defaultMove : K.Move
defaultMove =
    K.defaultMove


startPos : K.Move
startPos =
    { defaultMove | pos = ( 6, 2 ) }


endPos : K.Move
endPos =
    { defaultMove | pos = ( 7, 7 ) }


initialModel : Model
initialModel =
    { board = getBoard board_length
    , startPosInputValue = K.moveToAlgebraic startPos
    , endPosInputValue = K.moveToAlgebraic endPos
    , knightBFSModel = K.getInit startPos endPos board_length
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
    | CalculatingKnightMoves K.Msg
    | CalculateShortestPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStartPos value ->
            ( updatePos value updateStartPosInputValue updateKStartPos model, Cmd.none )

        UpdateEndPos value ->
            ( updatePos value updateEndPosInputValue updateKEndPos model, Cmd.none )

        UpdateBoardLength value ->
            let
                boardLength =
                    String.toInt value |> Maybe.withDefault board_length

                newModel =
                    if boardLength >= board_length then
                        { model
                            | knightBFSModel =
                                Tuple.first <|
                                    K.update
                                        (K.UpdateBoardLength boardLength)
                                        model.knightBFSModel
                            , board = getBoard boardLength
                        }

                    else
                        model
            in
            ( newModel, Cmd.none )

        CalculateShortestPath ->
            ( model
            , Cmd.map CalculatingKnightMoves <|
                K.getKnightMoves model.knightBFSModel
            )

        CalculatingKnightMoves msgK ->
            let
                ( modelK, cmdK ) =
                    K.update msgK model.knightBFSModel
            in
            ( { model | knightBFSModel = modelK }
            , Cmd.map CalculatingKnightMoves cmdK
            )


updatePos :
    String
    -> (String -> Model -> Model)
    -> (K.Move -> K.Model -> K.Model)
    -> Model
    -> Model
updatePos value posUpdater kPosUpdater model =
    let
        newModel =
            posUpdater value model
    in
    case K.algebraicToMove model.knightBFSModel.boardLength value of
        Ok move ->
            { newModel
                | knightBFSModel = kPosUpdater move model.knightBFSModel
                , error = Nothing
            }

        Err error ->
            { newModel
                | startPosInputValue = value
                , error = Just error
            }


updateStartPosInputValue : String -> Model -> Model
updateStartPosInputValue value model =
    { model | startPosInputValue = value }


updateEndPosInputValue : String -> Model -> Model
updateEndPosInputValue value model =
    { model | endPosInputValue = value }


updateKStartPos : K.Move -> K.Model -> K.Model
updateKStartPos move kModel =
    Tuple.first <| K.update (K.UpdateStartPos move) kModel


updateKEndPos : K.Move -> K.Model -> K.Model
updateKEndPos move kModel =
    Tuple.first <| K.update (K.UpdateEndPos move) kModel



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Visualizing Breadth First Search - Knight shortest path" ]
        , div []
            [ span [] [ text "Number of moves: " ]
            , span []
                [ text <|
                    case model.error of
                        Just error ->
                            error

                        Nothing ->
                            String.fromInt model.knightBFSModel.numberOfMoves
                ]
            ]
        , div [ class "config-container" ]
            [ div []
                [ span [] [ text "Board length" ]
                , input
                    [ onInput UpdateBoardLength
                    , type_ "number"
                    , value <| String.fromInt model.knightBFSModel.boardLength
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
            , button [ onClick CalculateShortestPath ] [ text "Get shortest path" ]
            ]
        , div
            [ class "board-container"
            , style "display" "grid"
            , style "grid-template-columns" "repeat(9, 50px)"
            , style "grid-template-rows" "repeat(9, 50px)"
            , style
                "grid-template-areas"
                (getGridTemplateAreas model.knightBFSModel.boardLength)
            ]
            [ div
                [ class "rows-label"
                , style "display" "grid"
                , style "grid-template-columns" "50px"
                , style "grid-template-rows" <| "repeat(" ++ String.fromInt model.knightBFSModel.boardLength ++ ", 50px)"
                , style "grid-area" "row"
                ]
              <|
                List.indexedMap
                    (\i _ -> div [] [ text <| String.fromInt <| i + 1 ])
                    (getBoardSlice model.knightBFSModel.boardLength)
            , div
                [ class "columns-label"
                , style "display" "grid"
                , style "grid-template-columns" <| "repeat(" ++ String.fromInt model.knightBFSModel.boardLength ++ ", 50px)"
                , style "grid-template-rows" "50px"
                , style "grid-area" "column"
                ]
              <|
                List.indexedMap
                    (\i _ ->
                        div []
                            [ text (i + K.charACode |> Char.fromCode >> String.fromChar) ]
                    )
                    (getBoardSlice model.knightBFSModel.boardLength)
            , div
                [ style "display" "grid"
                , style "grid-template-columns" <| "repeat(" ++ String.fromInt model.knightBFSModel.boardLength ++ ", 50px)"
                , style "grid-template-rows" <| "repeat(" ++ String.fromInt model.knightBFSModel.boardLength ++ ", 50px)"
                , style "grid-area" "board"
                , style "width" <| String.fromInt (model.knightBFSModel.boardLength * 50) ++ "px"
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
                                            getCellBackground i j model.knightBFSModel
                                        , style "border" <|
                                            getCellBorder i j model.knightBFSModel
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


getCellBackground : Int -> Int -> K.Model -> String
getCellBackground i j modelK =
    if ( i, j ) == modelK.startPos.pos then
        "blue"

    else if modelK.numberOfMoves > 0 && isInTrace i j modelK.currentPos.trace then
        "red"

    else if ( i, j ) == modelK.endPos.pos then
        "green"

    else if isInQueue i j modelK.queue then
        "yellow"

    else if modBy 2 (i + j) == 0 then
        "#769655"

    else
        "#edeed2"


isInQueue : Int -> Int -> K.Queue -> Bool
isInQueue i j queue =
    List.any
        (\move -> i == Tuple.first move.pos && j == Tuple.second move.pos)
        queue


isInTrace : Int -> Int -> K.Trace -> Bool
isInTrace i j (K.Trace trace) =
    List.any (\{ pos } -> pos == ( i, j )) trace


getCellBorder : Int -> Int -> K.Model -> String
getCellBorder i j { currentPos } =
    if i == Tuple.first currentPos.pos && j == Tuple.second currentPos.pos then
        "3px solid red"

    else
        "none"
