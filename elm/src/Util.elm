module Util exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Dict exposing (Dict)
import Debug
import Types exposing (..)

id x = x

simple :  (List (Attribute msg) -> List (Html msg) -> Html msg)
       -> String
       -> Html msg
simple f str = f [] [ text str ]

maybeToList : Maybe a -> List a
maybeToList mx =
  case mx of
    Nothing -> []
    Just x  -> [x]

multiDictFromList : (a -> comparable) -> List a -> Dict comparable (List a)
multiDictFromList key =
  List.foldl
    (adjustMultiDictEntry key)
    Dict.empty

adjustMultiDictEntry : (a -> comparable) -> a -> Dict comparable (List a) -> Dict comparable (List a)
adjustMultiDictEntry key val old = 
  Dict.update
    (key val)
    (\entry -> Just (val :: List.concat (maybeToList entry)))
    old

matchStringDec : String -> Decoder String
matchStringDec val =
  D.string |>
  D.andThen (\other -> if val == other then D.succeed val else D.fail ("expected " ++ val))

decSucceeds : Decoder a -> Decoder Bool
decSucceeds dec =
  D.oneOf [D.map (\_ -> True) dec, D.succeed False]

exactMatchDec : Decoder a -> a -> Decoder a
exactMatchDec dec val =
  dec |>
    D.andThen (\other ->
                 case val == other of
                     True  -> D.succeed val
                     False -> D.fail "mismatch in exactMatchDec")

decSet : b -> Decoder a -> Decoder b
decSet val = D.map (\_ -> val)

postcomp : (b -> c) -> (a -> b) -> (a -> c)
postcomp g f x = g (f x)

nubSorted : List a -> List a
nubSorted sortedList =
  case sortedList of
    x :: y :: ys -> 
      if x == y then nubSorted (y :: ys) else x :: nubSorted (y :: ys)
    _            ->
      sortedList

guardM : String -> (a -> Decoder Bool) -> Decoder a -> Decoder a
guardM errMsg predDec valDec =
  valDec |> D.andThen
    (\val ->
       predDec val |> D.andThen
         (\bool ->
            if bool then D.succeed val else D.fail errMsg))

formatModifier : Int -> String
formatModifier mod =
  case compare mod 0 of
    LT -> String.fromInt mod
    EQ -> " 0"
    GT -> "+" ++ String.fromInt mod

formatSnakeCase : String -> String
formatSnakeCase =
  String.split "_" >> List.intersperse " " >> String.concat

formatSnakeCaseCapitalized : String -> String
formatSnakeCaseCapitalized =
  formatSnakeCase >> stringMapFirst Char.toUpper

listMapFirst : (a -> a) -> List a -> List a
listMapFirst f l =
  case l of
      []    -> []
      x::xs -> f x :: xs

stringMapFirst : (Char -> Char) -> String -> String
stringMapFirst f s =
  case String.uncons s of
      Nothing      -> ""
      Just (c, cs) -> String.cons (f c) cs

prettyPrologTerm : PrologTerm -> String
prettyPrologTerm =
  Types.mapPrologTermStrings formatSnakeCase
    >> Types.foldPT
      (\f rs -> String.concat [f, " (", String.concat (List.intersperse ", " rs), ")"])
      id
