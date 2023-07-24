module Util exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Dict exposing (Dict)
import Debug

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
