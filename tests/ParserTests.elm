module ParserTests exposing (suite)

import Expect
import Main exposing (buffParser, dieParser, parser, probGenToTuple)
import Parser exposing (run)
import Test exposing (..)


suite : Test
suite =
    describe "Die parser test"
        [ test "For 2d6" <|
            \_ ->
                run dieParser "2d6"
                    |> Result.map probGenToTuple
                    |> Result.withDefault ( 0, 0 )
                    |> Expect.equal ( 2, 6 )
        , test "For +1" <|
            \_ ->
                run buffParser "+1"
                    |> Result.map probGenToTuple
                    |> Result.withDefault ( 0, 0 )
                    |> Expect.equal ( 1, 1 )
        , test "For -3" <|
            \_ ->
                run buffParser "-3"
                    |> Result.map probGenToTuple
                    |> Result.withDefault ( 0, 0 )
                    |> Expect.equal ( -3, 1 )
        , test "For '2d6+4'" <|
            \_ ->
                run parser "2d6+4"
                    |> Result.map (List.map probGenToTuple)
                    |> Result.withDefault []
                    |> Expect.equal [ ( 2, 6 ), ( 4, 1 ) ]
        ]
