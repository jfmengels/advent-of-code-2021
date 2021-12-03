module MainHelper exposing (Main, Part, Problem, program)

import Html exposing (Html)


type alias Problem =
    { day : Int
    , part1 : Part
    , part2 : Maybe Part
    , exampleInput : String
    , myInput : String
    }


type alias Part =
    { exampleSolution : Int
    , solve : String -> Int
    }


type alias Main =
    Html ()


program : Problem -> Main
program problem =
    Html.main_ []
        [ Html.h1 [] [ Html.text ("Day " ++ String.fromInt problem.day) ]
        , viewPart problem 1 problem.part1
        , case problem.part2 of
            Just part2 ->
                viewPart problem 2 part2

            Nothing ->
                Html.text ""
        ]


viewPart : Problem -> Int -> Part -> Html msg
viewPart problem partNumber part =
    let
        solutionOnExample =
            part.solve problem.exampleInput
    in
    Html.p
        []
        [ Html.text ("Solution for part " ++ String.fromInt partNumber ++ ": ")
        , Html.ul []
            [ Html.li []
                [ Html.text "on example data: "
                , Html.text (String.fromInt solutionOnExample)
                , if solutionOnExample == part.exampleSolution then
                    Html.text " (as expected)"

                  else
                    Html.text " (NOT as expected)"
                ]
            , Html.li []
                [ Html.text "on my input: "
                , Html.text (String.fromInt (part.solve problem.myInput))
                ]
            ]
        ]
