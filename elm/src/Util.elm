module Util exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Dict exposing (Dict)
import Maybe exposing (Maybe)
import Set exposing (Set)
import Debug
import Types exposing (..)

id x = x

applyIfPresent : Maybe a -> (a -> b -> b) -> b -> b
applyIfPresent mx f = Maybe.withDefault id (Maybe.map f mx)

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

yesNoDec : Decoder Bool
yesNoDec =
  D.oneOf
    [ matchStringDec "yes" |> D.map (\_ -> True)
    , matchStringDec "no" |> D.map (\_ -> False)
    ]
  

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
    _  -> "+" ++ String.fromInt mod

formatSnakeCase : String -> String
formatSnakeCase =
  String.split "_" >> List.intersperse " " >> String.concat

formatSnakeCaseCapitalized : String -> String
formatSnakeCaseCapitalized =
  formatSnakeCase >> stringMapFirst Char.toUpper

showPrologTermAlt : PrologTerm -> String
showPrologTermAlt = showPrologTerm_ formatSnakeCase " (" ")"

showPrologTerm : PrologTerm -> String
showPrologTerm = showPrologTerm_ (\x -> x) "(" ")"

showPrologTerm_ : (String -> String) -> String -> String -> PrologTerm -> String
showPrologTerm_ formatString openParen closeParen t =
  let spt = showPrologTerm_ formatString openParen closeParen
      showCompound f args = formatString f
        ++ openParen
        ++ String.concat (List.intersperse ", " (List.map spt args))
        ++ closeParen
  in case t of
    Atomic atom -> formatString atom
    List xs -> String.concat ["[", String.concat (List.intersperse ", " (List.map spt xs)), "]"]
    Compound "in" [s] ->
      "in " ++ spt s
    Compound "damage" [Atomic ty, damage] ->
      spt damage ++ " " ++ ty ++ " damage"
    Compound "dc" [Atomic ability, Atomic dc] ->
      "DC " ++ dc ++ " " ++ String.toUpper ability
    Compound "/" [Atomic "1", Atomic "2"] ->
      "1 / 2" -- TODO unicode
    Compound binOp [n, m] ->
      if Set.member binOp prologTermInfixOperators
      then String.concat [spt n, " ", binOp, " ", spt m]
      else showCompound binOp [n, m]
    -- Compound "+" [n, m] ->
    --   spt n ++ " + " ++ spt m
    -- Compound ":" [n, m] ->
    --   spt n ++ " : " ++ spt m
    Compound "cr" [val] ->
      "CR " ++ spt val
    Compound f args ->
      showCompound f args

prologTermInfixOperators : Set String
prologTermInfixOperators = Set.fromList ["+", ":", "by", "ft", "d", "else"]


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
      (\xs   -> String.concat ["[", String.concat (List.intersperse ", " xs), "]"])
      id

ordinal : Int -> String
ordinal n =
  case n of
    1 -> "1st"
    2 -> "2nd"
    3 -> "3rd"
    _ -> String.fromInt n ++ "th"

split : Int -> List a -> (List a, List a)
split n list =
  case (n, list) of
    ( _ , []      ) -> ([], [])
    ( 0 , _       ) -> ([], list)
    ( _ , x :: xs ) -> let ( ys , zs ) = split (n-1) xs
                       in ( x :: ys , zs )

chunks : Int -> List a -> List (List a)
chunks chunkSize list =
  case split chunkSize list of
    ( hd , [] ) -> [ hd ]
    ( hd , tl ) -> hd :: chunks chunkSize tl

classAbbrev : String -> String
classAbbrev className =
  case className of
      "barbarian" -> "bb"
      "bard"      -> "bd"
      "cleric"    -> "cl"
      "druid"     -> "dr"
      "fighter"   -> "fi"
      "monk"      -> "mo"
      "paladin"   -> "pa"
      "ranger"    -> "ra"
      "rogue"     -> "ro"
      "sorcerer"  -> "so"
      "warlock"   -> "wl"
      "wizard"    -> "wz"
      _           -> "??"
