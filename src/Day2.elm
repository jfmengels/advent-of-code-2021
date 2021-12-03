module Day2 exposing (main)

import MainHelper


main : MainHelper.Main
main =
    MainHelper.program
        { day = 2
        , part1 =
            { exampleSolution = 150
            , solve = solvePart1
            }
        , part2 =
            Just
                { exampleSolution = 900
                , solve = solvePart2
                }
        , myInput = myInput
        , exampleInput = """
forward 5
down 5
forward 8
up 3
down 8
forward 2
"""
        }


solvePart1 : String -> Int
solvePart1 input =
    let
        instructions : List InstructionPart1
        instructions =
            input
                |> String.trim
                |> String.split "\n"
                |> List.filterMap parseInstruction_part1

        ( finalX, finalY ) =
            List.foldl
                (\instruction ( horizontal, depth ) ->
                    case instruction of
                        Part1_Forward x ->
                            ( horizontal + x, depth )

                        Part1_AlterDepth y ->
                            ( horizontal, depth + y )
                )
                ( 0, 0 )
                instructions
    in
    finalX * finalY


type InstructionPart1
    = Part1_Forward Int
    | Part1_AlterDepth Int


parseInstruction_part1 : String -> Maybe InstructionPart1
parseInstruction_part1 str =
    case String.split " " str of
        [ instruction, value ] ->
            case String.toInt value of
                Just int ->
                    case instruction of
                        "forward" ->
                            Just (Part1_Forward int)

                        "down" ->
                            Just (Part1_AlterDepth int)

                        "up" ->
                            Just (Part1_AlterDepth -int)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


solvePart2 : String -> Int
solvePart2 input =
    let
        instructions : List InstructionPart2
        instructions =
            input
                |> String.trim
                |> String.split "\n"
                |> List.filterMap parseInstruction

        ( finalX, finalY, _ ) =
            List.foldl
                (\instruction ( horizontal, depth, aim ) ->
                    case instruction of
                        Forward_Part2 x ->
                            ( horizontal + x, depth + (aim * x), aim )

                        AlterAim_Part2 y ->
                            ( horizontal, depth, aim + y )
                )
                ( 0, 0, 0 )
                instructions
    in
    finalX * finalY


type InstructionPart2
    = Forward_Part2 Int
    | AlterAim_Part2 Int


parseInstruction : String -> Maybe InstructionPart2
parseInstruction str =
    case String.split " " str of
        [ instruction, value ] ->
            case String.toInt value of
                Just int ->
                    case instruction of
                        "forward" ->
                            Just (Forward_Part2 int)

                        "down" ->
                            Just (AlterAim_Part2 int)

                        "up" ->
                            Just (AlterAim_Part2 -int)

                        _ ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


myInput : String
myInput =
    """
forward 6
down 2
forward 2
down 8
forward 3
down 6
down 8
down 9
forward 7
forward 8
down 9
down 8
down 9
up 8
forward 1
down 7
down 3
forward 3
forward 1
down 3
forward 3
forward 1
up 8
down 5
down 1
forward 6
forward 2
up 9
down 3
down 8
down 3
down 3
up 2
down 7
down 3
up 5
forward 4
down 9
forward 6
forward 3
forward 1
forward 3
down 2
up 9
down 4
forward 6
down 3
forward 2
down 2
up 5
up 1
forward 3
forward 6
down 6
forward 7
forward 1
down 3
down 8
forward 2
down 7
up 1
up 2
forward 5
down 8
down 8
forward 9
forward 7
forward 2
forward 7
up 6
up 9
down 4
forward 4
forward 4
up 1
down 7
forward 9
forward 3
down 6
down 9
forward 7
forward 4
up 7
up 6
up 8
down 9
forward 1
down 1
forward 8
down 7
forward 5
down 3
down 3
down 8
down 8
down 4
up 4
forward 3
down 8
down 9
up 3
up 8
down 9
up 5
forward 2
forward 5
forward 5
down 8
forward 9
forward 8
down 5
down 9
forward 6
forward 2
forward 3
up 1
forward 1
up 2
up 2
forward 4
forward 8
forward 5
down 1
up 4
forward 5
up 7
down 5
down 5
forward 8
up 2
down 7
down 6
down 5
down 5
down 1
down 8
forward 9
forward 2
up 6
up 4
down 8
forward 1
forward 2
down 2
forward 7
forward 7
forward 3
forward 6
forward 8
down 3
forward 6
up 5
down 3
down 8
up 1
forward 1
down 7
down 3
up 5
forward 6
forward 8
forward 9
up 5
up 5
up 5
forward 8
up 5
down 6
down 7
down 5
up 7
up 1
up 3
forward 8
up 9
down 7
down 4
up 6
up 8
up 9
up 9
forward 5
up 5
forward 2
forward 2
forward 6
up 2
down 8
up 2
forward 5
down 9
up 7
down 9
forward 1
forward 8
up 1
forward 7
forward 2
down 3
forward 3
forward 2
up 9
forward 4
forward 9
down 9
forward 5
forward 1
forward 5
forward 8
up 5
forward 1
down 4
up 8
up 4
up 7
forward 4
down 1
up 6
forward 6
down 2
down 7
forward 4
up 7
forward 7
forward 9
down 5
up 5
forward 4
down 6
forward 1
up 8
up 8
down 8
down 7
forward 7
down 3
forward 7
down 3
down 5
down 4
up 8
down 2
down 2
up 5
forward 9
up 9
forward 2
up 4
forward 4
down 2
down 7
forward 7
down 1
down 6
down 4
forward 6
up 4
forward 4
down 6
down 8
down 3
forward 7
down 3
forward 7
down 7
forward 4
up 9
down 5
forward 7
forward 7
up 6
down 3
forward 9
down 1
forward 4
up 9
down 3
up 9
down 5
up 6
forward 1
forward 9
up 4
down 3
forward 1
down 7
down 2
forward 2
down 6
up 4
down 4
up 9
down 3
down 9
down 4
down 1
up 8
down 2
up 1
forward 5
forward 9
forward 1
up 4
forward 5
down 7
up 6
down 3
forward 8
down 1
down 5
forward 5
down 5
down 7
down 8
down 7
up 6
forward 8
down 8
forward 6
down 6
down 7
down 3
forward 2
down 6
down 8
down 7
down 3
up 1
down 7
forward 8
forward 2
forward 5
down 4
up 4
forward 9
down 9
forward 6
down 7
down 4
down 8
up 9
forward 7
down 4
forward 7
forward 1
forward 7
down 9
down 7
forward 3
forward 3
forward 2
down 5
up 5
forward 5
down 2
forward 7
forward 9
forward 7
down 7
down 9
down 5
forward 2
up 5
down 3
forward 7
down 4
down 3
up 5
down 6
down 3
up 4
forward 3
down 1
forward 6
forward 6
down 8
forward 9
down 2
up 3
down 4
down 5
forward 3
down 9
forward 2
up 3
up 4
forward 9
down 2
forward 9
forward 3
down 4
down 2
down 5
down 4
forward 4
down 1
down 9
down 2
forward 8
down 5
forward 5
up 7
down 5
down 2
forward 5
up 4
down 5
up 3
forward 7
down 9
forward 5
forward 2
forward 1
down 7
down 9
down 2
up 2
up 2
up 4
down 4
down 7
down 3
forward 5
forward 3
up 6
down 6
up 6
up 9
forward 8
forward 4
up 3
forward 1
forward 2
up 5
forward 5
forward 8
forward 7
forward 4
down 1
down 8
down 1
forward 3
up 1
forward 7
forward 4
down 8
forward 7
forward 9
forward 3
down 9
down 9
down 3
up 6
up 1
down 4
forward 5
forward 4
forward 6
forward 8
down 6
down 3
forward 5
forward 6
down 4
down 2
up 3
down 3
down 7
down 5
down 5
forward 6
down 4
forward 1
up 2
forward 3
down 1
down 4
down 9
down 7
down 9
forward 9
down 6
down 3
down 2
down 5
up 8
forward 5
forward 5
forward 4
up 5
forward 1
down 9
down 1
up 5
forward 8
forward 6
forward 5
down 1
up 5
down 8
up 7
down 8
down 2
down 3
forward 2
up 4
down 6
up 6
down 3
down 7
up 3
forward 4
down 3
forward 4
up 9
forward 5
down 2
forward 7
forward 5
up 3
up 2
forward 2
down 8
down 1
down 3
up 5
down 4
forward 4
down 1
forward 9
down 3
down 7
down 4
down 4
forward 7
up 5
forward 4
down 8
up 4
forward 6
down 1
up 4
forward 4
down 6
up 5
up 1
forward 2
down 5
forward 8
forward 6
down 8
down 7
down 7
down 1
forward 5
forward 7
forward 7
forward 7
up 3
forward 9
forward 1
down 9
forward 4
up 8
forward 1
forward 5
forward 4
down 2
forward 4
forward 9
forward 3
down 1
forward 4
forward 9
forward 5
down 5
down 5
forward 7
down 3
forward 4
down 6
forward 7
down 2
down 1
down 5
forward 4
forward 9
down 4
forward 2
down 8
up 5
down 9
forward 8
down 3
up 6
down 2
down 4
forward 4
up 2
down 4
down 4
up 7
down 6
forward 4
down 7
forward 3
down 1
up 1
down 2
down 6
down 4
up 3
down 6
up 2
down 6
forward 3
down 9
forward 5
down 5
down 9
down 9
down 7
forward 9
forward 8
forward 9
up 9
forward 7
forward 4
forward 4
up 5
forward 2
down 1
up 9
forward 2
forward 7
forward 1
down 9
forward 9
up 8
up 1
up 7
up 7
down 5
forward 2
forward 8
forward 6
down 7
forward 1
down 9
down 4
down 4
down 1
up 7
forward 4
forward 6
up 5
forward 2
down 9
down 7
forward 1
forward 2
down 5
forward 3
forward 8
forward 6
forward 3
forward 2
down 1
forward 1
forward 1
forward 3
down 9
up 9
down 9
down 6
forward 7
down 6
forward 9
down 9
down 7
down 1
down 9
up 9
down 6
forward 9
down 6
forward 3
down 8
up 5
forward 5
forward 8
up 3
down 8
up 6
forward 4
down 2
forward 6
down 9
forward 6
forward 4
forward 9
forward 3
down 2
down 4
forward 5
down 9
up 7
forward 4
up 1
forward 1
down 6
forward 3
forward 7
forward 2
forward 2
down 5
down 9
down 3
down 5
up 3
forward 1
down 2
down 4
down 1
up 9
up 5
up 1
down 1
up 9
down 5
up 3
up 3
down 7
forward 4
down 6
forward 2
forward 7
forward 4
down 2
forward 6
forward 2
down 3
up 3
up 9
forward 9
forward 9
forward 6
down 8
down 1
forward 9
up 1
down 6
forward 6
up 5
forward 2
forward 6
down 9
forward 1
forward 8
down 8
forward 4
forward 7
up 6
up 1
forward 7
forward 3
forward 2
down 4
down 7
down 7
down 1
down 6
forward 1
down 9
up 9
up 9
down 2
down 2
forward 5
up 2
forward 7
up 5
down 9
forward 7
forward 2
down 8
up 1
down 5
forward 6
down 8
down 7
forward 4
up 2
down 8
forward 2
down 5
down 4
down 9
down 1
down 9
down 6
down 3
forward 1
forward 6
up 1
up 1
up 9
down 2
down 2
forward 5
down 3
forward 4
down 3
down 7
down 7
forward 4
up 3
forward 4
down 3
forward 8
forward 1
up 2
up 1
forward 1
down 6
down 1
down 3
forward 7
down 7
forward 4
forward 5
forward 3
down 5
forward 9
forward 5
down 7
forward 6
down 4
down 4
down 9
down 3
up 9
forward 7
down 7
forward 6
down 2
down 9
forward 4
forward 1
forward 4
down 5
forward 7
down 9
down 8
forward 9
forward 1
down 9
forward 6
up 5
forward 9
down 1
down 5
forward 4
forward 5
forward 8
down 5
forward 9
down 6
down 2
up 4
up 8
forward 3
forward 4
down 3
forward 4
up 6
forward 3
forward 8
forward 7
down 1
down 9
down 8
down 8
down 1
forward 9
up 4
down 5
forward 7
down 8
down 3
forward 9
down 5
forward 7
forward 2
down 4
forward 2
forward 7
down 6
forward 7
down 2
forward 9
down 9
forward 8
forward 8
down 6
forward 7
down 8
forward 7
forward 3
down 1
up 8
down 5
down 6
up 5
forward 5
forward 5
up 5
up 3
up 7
down 6
forward 8
forward 4
down 2
up 5
forward 8
down 6
forward 4
forward 2
up 8
down 8
down 5
down 4
forward 9
forward 9
forward 6
forward 6
down 3
up 1
down 4
down 8
down 9
down 1
forward 3
forward 1
down 9
down 3
down 7
forward 6
forward 9
down 8
down 8
forward 6
forward 1
down 3
forward 1
down 8
down 3
down 9
up 1
forward 6
up 2
down 3
forward 4
forward 2
up 2
down 5
forward 1
down 3
forward 9
forward 4
forward 6
down 3
forward 7
down 6
up 3
up 7
up 5
down 4
forward 4
up 1
forward 7
up 9
forward 3
up 1
down 3
down 4
forward 4
up 3
down 6
down 9
down 6
forward 4
down 9
down 6
forward 4
forward 3
down 3
up 7
down 9
forward 8
"""
