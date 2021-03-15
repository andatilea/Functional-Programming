-----------------------
-- Anda-Corina Tilea
-- 28.10.2020
-----------------------

module Solutions exposing (..)
import List exposing (sort, take, member,drop, isEmpty)
import String exposing (toList)


-- 2.2.1
type Face = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King

type Suit = Clubs | Diamonds | Hearts | Spades

type Card = Card Face Suit


fullFace : Face -> List Card

fullFace f = [Card f Clubs, Card f Diamonds, Card f Hearts, Card f Spades]      -- a list with all the values a Card's Face could have;

deck : List Card    -- to obtain a list with all the Cards, I will concat the fullFace lists for each of the possible Faces;

deck =
    List.concat [fullFace Ace, fullFace Two, fullFace Three, fullFace Four, fullFace Five, fullFace Six, fullFace Seven, fullFace Eight
                            , fullFace Nine, fullFace Ten, fullFace Jack, fullFace Queen, fullFace King]


--2.2.2

cardValue : Card -> List Int

cardValue c =           -- we are interested only in the Face of the Cards, for each Face a certain value will be added to the list;
    case c of
        (Card Two _ ) -> 2 :: []
        (Card Three _ ) -> 3 :: []
        (Card Four _ ) -> 4 :: []
        (Card Five _ ) -> 5 :: []
        (Card Six _ ) -> 6 :: []
        (Card Seven _ ) ->  7 :: []
        (Card Eight _ ) -> 8 :: []
        (Card Nine _ ) -> 9 :: []
        (Card Ten _ ) -> 10 :: []
        (Card Jack _ ) -> 10 :: []
        (Card Queen _ ) -> 10 :: []
        (Card King _ ) -> 10 :: []
        (Card Ace _ ) -> 1 :: 11 :: []


-- 2.2.3 -> assuming that k > 0

smallestK : Int -> List number -> List number

smallestK k lx =

     case lx of
         x::xs -> take k (sort lx)       -- if the list is not empty -> it will be sorted for extracting the first k elements;
         others -> []                        -- if the list is empty, return [];

--2.2.4

balanced : String -> Bool
balanced s =

        let match : List Char -> Int -> Bool
            match c open =             -- create a helper function to check all the parentheses appearances;
                    case c of
                        [] -> open == 0
                        ')' :: xs -> open > 0 && match xs (open - 1)
                        '(' :: xs -> match xs (open + 1)
                        _ :: xs -> match xs open
        in
            match (toList s) 0          -- calling the helper function for the input string (initialize the counter to 0);


-- 2.2.5

element: List Int -> Int                -- helper for finding the first element of a list in Int form;
element l =
    case l of
        x :: _ -> x
        others -> 0

coinChange: Int -> List Int -> Int
coinChange a l =
     if a <= 1  then
        if a == 0 || member a l then 1
        else 0
     else if isEmpty l then 0
     else
        coinChange (a - element(take 1 l)) l  + coinChange a (drop 1 l)   -- apply it recursively after taking the first element from the input list;