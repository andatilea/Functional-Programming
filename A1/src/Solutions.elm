module Solutions exposing (..)

-----------------------
-- Anda-Corina Tilea
-- 12.10.2020
-----------------------

--1.2.1

type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

type Suit = Clubs | Diamonds | Hearts | Spades

type Card = Card Face Suit

-- 1.2.2

faceToString : Face -> String

faceToString f =
    case f of
        Ace -> "Ace"
        Two -> "Two"
        Three -> "Three"
        Four -> "Four"
        Five -> "Five"
        Six -> "Six"
        Seven -> "Seven"
        Eight -> "Eight"
        Nine -> "Nine"
        Ten -> "Ten"
        Jack -> "Jack"
        Queen -> "Queen"
        King -> "King"


suitToString : Suit -> String

suitToString s =
    case s of
        Clubs -> "Clubs"
        Diamonds -> "Diamonds"
        Hearts -> "Hearts"
        Spades -> "Spades"


cardToString : Card -> String

cardToString c =
    let (Card face suit) = c
    in
    faceToString(face) ++ " of " ++ suitToString(suit)


--1.2.3

type alias Point = {x : Float, y : Float}
type Segment = Segment Point Point

linesIntersect: Segment -> Segment -> Bool

linesIntersect seg1 seg2 =
   let
    (Segment p1 p2) = seg1
    (Segment q1 q2) = seg2
    a = q1.x - p1.x
    b = p2.y - p1.y
    c = q1.y - p1.y
    d = p2.x - p1.x
    e = q2.x - p1.x
    f = q2.y - p1.y
    g = (-a)
    h = q2.y - q1.y
    i = (-c)
    j = q2.x - q1.x
    k = p2.x - q1.x
    m = p2.y - q1.y
   in
   -- True -> The two segments intersect
   -- False -> The two segments will not intersect
    ((a * b - c * d) * (e * b - f * d) < 0) && ((g * h - i * j) * (k * h - m * j) < 0)



--1.2.4

trailingZeroes : Int -> Int

trailingZeroes z =
    let
        rec: Int -> Int
        rec n =
            if n == 0 then 0
            else
           n // 5 +  rec(n // 5)

    in
    rec z







