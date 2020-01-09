module KnightBFS exposing (Model, Move, Msg(..), Queue, algebraicToMove, charACode, defaultError, getInit, getKnightMoves, moveToAlgebraic, update)

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
    , finishPos : Move
    , currentPos : Move
    , boardLength : BoardLength
    , numberOfMoves : Int
    , queue : Queue
    }


type alias Move =
    ( Int, Int, Int )


type alias BoardLength =
    Int


type alias Queue =
    List Move


getInit : Move -> Move -> BoardLength -> Model
getInit startPos finishPos boardLength =
    { startPos = startPos
    , finishPos = finishPos
    , currentPos = startPos
    , boardLength = boardLength
    , numberOfMoves = 0
    , queue = []
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
getKnightMoves ({ currentPos, finishPos, boardLength, queue } as model) =
    let
        ( currentI, currentJ, currentLevel ) =
            currentPos

        ( finishI, finishJ, _ ) =
            finishPos
    in
    if currentI == finishI && currentJ == finishJ then
        Task.perform CalculationDone <|
            Task.succeed currentLevel

    else
        let
            newQueue =
                getPossibleMoves currentPos
                    |> List.filterMap (validateMove boardLength)
                    |> List.filter (isUniqueInQueue queue)
                    |> getNextQueue queue

            nextPos =
                newQueue |> List.head |> Maybe.withDefault ( 0, 0, 0 )
        in
        Task.perform
            Calculating
            (Process.sleep 1000
                |> Task.andThen
                    (\_ ->
                        Task.succeed
                            { model
                                | currentPos = nextPos
                                , queue = newQueue
                            }
                    )
            )


moveToAlgebraic : Move -> String
moveToAlgebraic ( i, j, _ ) =
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
        -- (2, 0, 0)
        |> Maybe.map addDefaultLevel
        -- Maybe (2, 0, 0)
        |> Maybe.andThen (validateMove boardLength)
        -- Result "Wrong position" (2, 0, 0)
        |> Result.fromMaybe defaultError


parseRow : ( Char, String ) -> Maybe ( Int, Char )
parseRow ( j, i ) =
    i
        |> String.toInt
        |> Maybe.map (\int -> ( int - 1, j ))


parseColumn : ( Int, Char ) -> ( Int, Int )
parseColumn ( i, j ) =
    ( i, Char.toCode j - charACode )


addDefaultLevel : ( Int, Int ) -> Move
addDefaultLevel ( i, j ) =
    ( i, j, 0 )


validateMove : Int -> Move -> Maybe Move
validateMove boardLength ( i, j, l ) =
    if i >= 0 && i < boardLength && j >= 0 && j < boardLength then
        Just ( i, j, l )

    else
        Nothing


getNextQueue : Queue -> Queue -> Queue
getNextQueue queue possibleMoves =
    possibleMoves
        |> (++) queue
        |> List.tail
        |> Maybe.withDefault []


isUniqueInQueue : Queue -> Move -> Bool
isUniqueInQueue queue move =
    List.member move queue |> not


getPossibleMoves : Move -> Queue
getPossibleMoves ( i, j, level ) =
    List.map2
        (\iMove jMove -> ( i + iMove, j + jMove, level + 1 ))
        -- possible moves for i
        [ -2, -2, -1, 1, 2, 2, -1, 1 ]
        -- possible moves for j
        [ -1, 1, 2, 2, -1, 1, -2, -2 ]
