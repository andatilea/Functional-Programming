module RegexTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import Regex exposing (..)

genTest name fn input expected =
  test name <|
    \_ -> Expect.equal expected (fn input)

suite : Test
suite =
  describe "Regex" [
    describe "matchLit" [
      genTest "matchLit 1" (matchLit 'a') ['a', 'b', 'b'] (Ok (['a'], ['b', 'b'])),
      genTest "matchLit 2" (matchLit 'c') ['a', 'b', 'b'] (Err ['a', 'b', 'b']),
      genTest "matchLit 3" (matchLit 'c') [] (Err [])
    ],
    describe "matchSeq" [
      genTest "matchSeq 1" (matchSeq (Literal 'a') (Literal 'b')) ['a', 'b', 'c'] (Ok (['a', 'b'], ['c'])),
      genTest "matchSeq 2" (matchSeq (Literal 'a') (Literal 'b')) ['a', 'x', 'c'] (Err (['a', 'x', 'c'])),
      genTest "matchSeq 3" (matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c')) ['a', 'b', 'c', 'd'] (Ok (['a', 'b', 'c'], ['d']))
    ],
    describe "matchMany" [
      genTest "matchMany 1" (matchMany (Literal 'a')) ['a', 'a', 'a'] (Ok (['a', 'a', 'a'], [])),
      genTest "matchMany 2" (matchMany (Literal 'b')) ['a', 'a', 'a'] (Ok ([], ['a', 'a', 'a'])),
      genTest "matchMany 3" (matchMany (Literal 'b')) ['b', 'b', 'a'] (Ok (['b', 'b'], ['a'])),
      genTest "matchMany 4" (matchMany (Seq (Literal 'b') (Literal 'a'))) ['b', 'a', 'b', 'a', 'c'] (Ok (['b', 'a', 'b', 'a'], ['c'])),
      genTest "matchMany 5" (matchMany (Seq (Literal 'b') (Literal 'a'))) ['b', 'a', 'c', 'a', 'c'] (Ok (['b', 'a'], ['c', 'a', 'c']))
    ],
    describe "matchOneOf" [
      genTest "matchOneOf 1" (matchOneOf (Literal 'a') (Literal 'b')) ['a', 'a', 'a'] (Ok (['a'], ['a', 'a'])),
      genTest "matchOneOf 2" (matchOneOf (Literal 'b') (Literal 'a')) ['a', 'a', 'a'] (Ok (['a'], ['a', 'a'])),
      genTest "matchOneOf 3" (matchOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))) ['c', 'd', 'a'] (Ok (['c', 'd'], ['a']))
    ]
  ]