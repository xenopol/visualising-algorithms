module KnightBFS exposing (Model, Move, Msg(..), Queue, Trace(..), algebraicToMove, charACode, defaultError, defaultMove, getInit, getKnightMoves, moveToAlgebraic, update)

import Process
import Task


charACode : Int
charACode =
    97


defaultError : String
defaultError =
    "Wrong positions"



-- Model


type alias Model =
    { startPos : Move
    , endPos : Move
    , currentPos : Move
    , boardLength : BoardLength
    , numberOfMoves : Int
    , queue : Queue
    , discardedQueue : Queue
    }


type alias Move =
    { pos : ( Int, Int )
    , level : Int
    , trace : Trace
    }


type Trace
    = Trace (List Move)


type alias BoardLength =
    Int


type alias Queue =
    List Move


defaultMove : Move
defaultMove =
    { pos = ( 0, 0 )
    , level = 0
    , trace = Trace []
    }


getInit : Move -> Move -> BoardLength -> Model
getInit startPos endPos boardLength =
    { startPos = startPos
    , endPos = endPos
    , currentPos = startPos
    , boardLength = boardLength
    , numberOfMoves = 0
    , queue = []
    , discardedQueue = []
    }



-- Update


type Msg
    = Calculating Model
    | CalculationDone Int
    | UpdateBoardLength BoardLength


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Calculating newModel ->
            ( newModel, getKnightMoves newModel )

        CalculationDone numberOfMoves ->
            ( { model | numberOfMoves = numberOfMoves }, Cmd.none )

        UpdateBoardLength length ->
            ( { model | boardLength = length }, Cmd.none )


getKnightMoves : Model -> Cmd Msg
getKnightMoves ({ currentPos, endPos, boardLength, queue, discardedQueue } as model) =
    if currentPos.pos == endPos.pos then
        Task.perform CalculationDone <|
            Task.succeed currentPos.level

    else
        let
            possibleMoves =
                getPossibleMoves currentPos
                    |> List.filterMap (validateMove boardLength)
                    |> List.filter (isUniqueInQueue queue)
                    |> List.filter (isUniqueInQueue discardedQueue)

            nextQueue =
                queue ++ possibleMoves
        in
        Task.perform
            Calculating
            (Process.sleep 100
                |> Task.andThen
                    (\_ ->
                        Task.succeed
                            { model
                                | currentPos = getQueueHead nextQueue
                                , queue = getQueueTail nextQueue
                                , discardedQueue = currentPos :: discardedQueue
                            }
                    )
            )


moveToAlgebraic : Move -> String
moveToAlgebraic { pos } =
    let
        ( i, j ) =
            pos
    in
    String.fromInt (i + 1)
        |> (++) (j + charACode |> Char.fromCode |> String.fromChar)


algebraicToMove : BoardLength -> String -> Result String Move
algebraicToMove boardLength position =
    -- a3
    position
        -- ('a', "3")
        |> String.uncons
        -- Maybe (2, 'a')
        |> Maybe.andThen parseRow
        -- (2, 0)
        |> Maybe.map parseColumn
        -- Move
        |> Maybe.map convertToMove
        -- Maybe Move
        |> Maybe.andThen (validateMove boardLength)
        -- Result "Wrong position" Move
        |> Result.fromMaybe defaultError


parseRow : ( Char, String ) -> Maybe ( Int, Char )
parseRow ( j, i ) =
    i
        |> String.toInt
        |> Maybe.map (\int -> ( int - 1, j ))


parseColumn : ( Int, Char ) -> ( Int, Int )
parseColumn ( i, j ) =
    ( i, Char.toCode j - charACode )


convertToMove : ( Int, Int ) -> Move
convertToMove pos =
    { defaultMove | pos = pos }


validateMove : Int -> Move -> Maybe Move
validateMove boardLength move =
    let
        ( i, j ) =
            move.pos
    in
    if i >= 0 && i < boardLength && j >= 0 && j < boardLength then
        Just move

    else
        Nothing


getQueueHead : Queue -> Move
getQueueHead queue =
    queue
        |> List.head
        |> Maybe.withDefault defaultMove


getQueueTail : Queue -> Queue
getQueueTail queue =
    queue
        |> List.tail
        |> Maybe.withDefault []


isUniqueInQueue : Queue -> Move -> Bool
isUniqueInQueue queue move =
    queue
        |> List.any (\{ pos } -> pos == move.pos)
        |> not


getPossibleMoves : Move -> Queue
getPossibleMoves move =
    let
        (Trace currentMoveTrace) =
            move.trace
    in
    List.map2
        (\i j ->
            { move
                | pos = ( Tuple.first move.pos + i, Tuple.second move.pos + j )
                , level = move.level + 1
                , trace = Trace <| move :: currentMoveTrace
            }
        )
        -- possible moves for i
        [ -2, -1, 1, 2, 2, 1, -1, -2 ]
        -- possible moves for j
        [ 1, 2, 2, 1, -1, -2, -2, -1 ]
