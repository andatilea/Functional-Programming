-----------------------
-- Anda-Corina Tilea
-- 28.10.2020
-----------------------
module Regex exposing (..)
import List exposing (take, drop, length, append)

type RegexPattern
  = Literal Char
  | Many RegexPattern
  | OneOf RegexPattern RegexPattern
  | Seq RegexPattern RegexPattern

{-
  The `Ok` variant represents the matched input and the rest of the unmatched input
  The `Err` variant represents the original input
-}
type alias RegexResult = Result (List Char) (List Char, List Char)

{-
  Returns the `Ok` variant if the literal character matches the first character of the string.
  If the string is empty or the characters don't match the `Err` variant should be returned.
  ```elm
  matchLit 'a' ['a', 'b', 'b'] == Ok (['a'], ['b', 'b'])
  matchLit 'c' ['a', 'b', 'b'] == Err ['a', 'b', 'b']
  matchLit 'c' [] == Err []
  ```
-}

--2.2.5

matchLit : Char -> List Char -> RegexResult
matchLit ch str =
    case str of
    x::xs -> if(x == ch) then Ok(x::[], xs)      -- if the character given is equal to the head of the list, then two lists will be created in the Ok variant (one with the match obtained and the rest of the list);
             else Err str                        -- if the match does not succeed, the Err variant will be returned, along with the input list;
    [] -> Err str

{-
  Matches `pat1` and then `pat2`. Returns `Ok` only if both succeed.
  ```elm
  matchSeq (Literal 'a') (Literal 'b') ['a', 'b', 'c'] == Ok (['a', 'b'], ['c'])
  matchSeq (Literal 'a') (Literal 'b') ['a', 'x', 'c'] == Err (['a', 'x', 'c'])
  matchSeq (Seq (Literal 'a') (Literal 'b')) (Literal 'c') ['a', 'b', 'c', 'd'] == Ok (['a', 'b', 'c'], ['d'])
  ```
-}

matchSeq : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchSeq pat1 pat2 input =

     case (pat1, pat2) of
     (Literal p1 , Literal p2) ->
                                      case input of
                                      (a::b::xs) -> if a == p1 then                          -- if pat1 is matched, we check to see if the pat2 also succeeds;
                                                    if b == p2 then Ok([a,b], xs)        -- if pat2 is also matched then we return the Ok variant, containing the solutions and the rest of the list;
                                                    else Err input                       -- if pat2 is not matched, then return Err variant;
                                                    else Err input                           -- if neither pat1 and pat2 are matched, the Err variant is returned;
                                      _ -> Err input

     ((Seq (Literal p1) (Literal p2)), (Literal p3)) ->
                                                          case input of
                                                          (a::b::c::xs) -> if a == p1 && b == p2 then                -- if pat1 is matched, we check pat2;
                                                                           if c == p3 then Ok([a,b,c], xs)       -- if pat2 is also matched -> Ok variant;
                                                                           else Err input                        -- if pat2 doesn't match, return Err variant;
                                                                           else Err input                            -- if no match is found, return Err variant;
                                                          _ -> Err input
     (_ , _ ) -> Err input      -- the other cases are not taken into consideration;

{-
  Matches the pattern `pattern` zero or many times. Always returns the `Ok` variant.
  ```elm
  matchMany (Literal 'a') ['a', 'a', 'a'] == Ok (['a', 'a', 'a'], [])
  matchMany (Literal 'b') ['a', 'a', 'a'] == Ok ([], ['a', 'a', 'a'])
  matchMany (Literal 'b') ['b', 'b', 'a'] == Ok (['b', 'b'], ['a'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'b', 'a', 'c'] == Ok (['b', 'a', 'b', 'a'], ['c'])
  matchMany (Seq (Literal 'b') (Literal 'a')) ['b', 'a', 'c', 'a', 'c'] == Ok (['b', 'a'], ['c', 'a', 'c'])
  ```
-}

matchMany : RegexPattern -> List Char -> RegexResult
matchMany pattern input =

    let
        pat_list =         case pattern of                                      -- considering the given pattern, we will create a pattern list in which the input elements will be stored;
                           (Literal p) -> p :: []                               -- if it's a Literal -> add the element in the list;
                           (Seq (Literal p1) (Literal p2)) -> p1::p2::[]        -- if as input we have a Seq then multiple elements are added;
                           _ -> []

        -- input1 will represent the problem's input list;
        helper input1 result = if (pat_list == (take (length pat_list) input1)) then          -- if there will be a match call the helper in order to form the results list;
                               helper (drop (length pat_list) input1) (append pat_list result)
                               else if result == [] then                                    -- if the list with matching results is empty, return the Ok variant with an empty list and the original input;
                               Ok(result, input1)
                               else Ok(result, (drop (length result) input))                  -- if some matches were found, the Ok variant will contain the matching characters and the rest of the original list;
    in

        helper input []                                                                       -- apply the helper on the problem's data;

{-
  Tries to match one of `pat1` and `pat2`, in this order. If `pat1` matches, its result is returned, else
  `pat2` is tried.
  ```elm
  matchOneOf (Literal 'a') (Literal 'b') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Literal 'b') (Literal 'a') ['a', 'a', 'a'] == Ok (['a'], ['a', 'a'])
  matchOneOf (Seq (Literal 'a') (Literal 'b')) (Seq (Literal 'c') (Literal 'd'))  ['c', 'd', 'a'] == (Ok (['c', 'd'], ['a']))
  ```
-}

matchOneOf : RegexPattern -> RegexPattern -> List Char -> RegexResult
matchOneOf pat1 pat2 input =

        case (pat1, pat2) of
        (Literal p1 , Literal p2) ->    case input of
                                        (a::xs) -> if p1 == a then              -- if pat1 is matched then we found a solution, so the result will be returned;
                                                   Ok(p1::[], xs)               -- the result contains the matching element and the rest of the list;
                                                   else if (p2 == a) then       -- if pat1 does not succeed -> pat2 is checked;
                                                   Ok(p2::[], xs)               -- if it matches then the result is returned (matching element, rest of list);
                                                   else Err input               -- else return the Err variant;
                                        _ -> Err input

        ((Seq (Literal p1) (Literal p2)), (Seq (Literal p3) (Literal p4))) ->   case input of
                                                                                (a::b::xs) -> if p1 == a && p2 == b then            -- if pat1 succeeds then return result;
                                                                                              Ok(p1::p2::[], xs)
                                                                                              else if p3 == a && p4 == b then       -- if pat1 does not succeed, we check pat2;
                                                                                              Ok(p3::p4::[], xs)                    -- if it's a match -> return result with Ok variant;
                                                                                              else Err input
                                                                                _ -> Err input                                      -- if there are no matches -> Err variant;
        (_) -> Err input            -- the other cases are not taken into consideration;


match : RegexPattern -> List Char -> RegexResult
match pattern input =
  case pattern of
    Literal char -> matchLit char input
    Many pat -> matchMany pat input
    OneOf pat1 pat2 -> matchOneOf pat1 pat2 input
    Seq pat1 pat2 -> matchSeq pat1 pat2 input
