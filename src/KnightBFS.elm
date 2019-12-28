module KnightBFS exposing (getKnightMoves)


charACode : Int
charACode =
    97


defaultError : String
defaultError =
    "Wrong positions"


type alias Queue =
    List Move


type alias Move =
    ( Int, Int, Int )


getKnightMoves : String -> String -> Int -> Result String Int
getKnightMoves start finish boardLength =
    let
        moves =
            [ start, finish ]
                |> List.map (algebraicToMove boardLength)
    in
    case moves of
        [ Ok startMove, Ok endMove ] ->
            [ startMove ]
                |> getShortestPath boardLength startMove endMove
                |> Ok

        _ ->
            Err defaultError


moveToAlgebraic : Move -> String
moveToAlgebraic ( i, j, _ ) =
    String.fromInt i
        |> (++) (j + charACode |> Char.fromCode |> String.fromChar)


algebraicToMove : Int -> String -> Result String Move
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


getShortestPath : Int -> Move -> Move -> Queue -> Int
getShortestPath boardLength (( iStart, jStart, level ) as start) (( iEnd, jEnd, _ ) as end) queue =
    if iStart == iEnd && jStart == jEnd then
        level

    else
        let
            possibleMoves =
                getPossibleMoves start boardLength queue

            newQueue =
                possibleMoves
                    |> (++) queue
                    |> List.tail
                    |> Maybe.withDefault []
        in
        getShortestPath
            boardLength
            (newQueue |> List.head |> Maybe.withDefault ( 0, 0, 0 ))
            end
            newQueue


isUniqueInQueue : Queue -> Move -> Bool
isUniqueInQueue queue move =
    List.member move queue
        |> not


getPossibleMoves : Move -> Int -> Queue -> Queue
getPossibleMoves ( i, j, l ) boardLength queue =
    [ -- up-left, up-right
      ( i - 2, j - 1, l + 1 )
    , ( i - 2, j + 1, l + 1 )

    -- right-up, right-down
    , ( i - 1, j + 2, l + 1 )
    , ( i + 1, j + 2, l + 1 )

    -- down-left, down-right
    , ( i + 2, j - 1, l + 1 )
    , ( i + 2, j + 1, l + 1 )

    -- left-up, left-down
    , ( i - 1, j - 2, l + 1 )
    , ( i + 1, j - 2, l + 1 )
    ]
        |> List.filterMap (validateMove boardLength)
        |> List.filter (isUniqueInQueue queue)
