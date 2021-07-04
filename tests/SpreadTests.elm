module SpreadTests exposing (suite)

import Expect
import Test exposing (..)
import Main exposing (spreadForListOfDice)


suite : Test
suite = 
  describe "Spread function returns reasonable values"
    [ test "For 1d6" <|
      \_ -> 
        spreadForListOfDice [6]
          |> Expect.equal [(1,1/6), (2,1/6), (3,1/6), (4,1/6), (5,1/6), (6,1/6)]

    , test "For 4d2 (coin flip)" <|
      \_ -> 
        spreadForListOfDice [2, 2, 2, 2]
          |> Expect.equal [(4,1/16), (5,1/4), (6,3/8), (7,1/4), (8,1/16)]


    ]
