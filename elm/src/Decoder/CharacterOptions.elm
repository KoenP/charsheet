module Decoder.CharacterOptions exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Dict exposing (Dict)

import Types exposing (..)
import Types.Ability exposing (..)
import Decoder.AbilityTable exposing (abilityTableDec)
import Decoder.PrologTerm exposing (prologTermDec)
import Util

gotCharacterOptionsDec : Decoder CharacterOptions
gotCharacterOptionsDec =
  D.succeed (\ability_table options traits_and_bonuses char_level ->
            { ability_table = ability_table
            , options = options
            , traits_and_bonuses = traits_and_bonuses
            , char_level = char_level
            })
    |> D.andMap (D.field "ability_table" abilityTableDec)
    |> D.andMap (D.field "options" optionsDictDec)
    |> D.andMap (D.field "traits_and_bonuses" traitsAndBonusesDictDec)
    |> D.andMap (D.field "char_level" D.int)

traitsAndBonusesDictDec : Decoder (Dict Level (List Effect))
traitsAndBonusesDictDec =
  intDictDec (D.list traitOrBonusDec)

traitOrBonusDec : Decoder Effect
traitOrBonusDec =
  D.succeed Effect
    |> D.andMap (D.field "effect" prologTermDec)
    |> D.andMap (D.field "pretty" D.string)
    |> D.andMap (D.field "origin" prologTermDec)
    |> D.andMap (D.field "desc"
                   (D.oneOf
                      [ D.map List.singleton D.string
                      , D.list D.string
                      , D.null []
                      ]))

optionsDictDec : Decoder (Dict Level (List Options))
optionsDictDec =
  intDictDec (D.list optionsDec)

intDictDec : Decoder a -> Decoder (Dict Int a)
intDictDec valueDec =
  D.keyValuePairs valueDec
    |> D.andThen
       (\kvPairs ->
          List.map
            (\(key, val) ->
               case D.decodeString D.int key of
                 Ok n    -> D.succeed (n, val)
                 Err err -> D.fail (D.errorToString err))
            kvPairs
          |> sequenceDecoders
          |> D.map Dict.fromList)

sequenceDecoders : List (Decoder a) -> Decoder (List a)
sequenceDecoders decs =
  case decs of
    []      -> D.succeed []
    d :: ds -> d |> D.andThen (\x -> sequenceDecoders ds |> D.andThen (\xs -> D.succeed (x :: xs)))

optionsDec : Decoder Options
optionsDec =
  D.succeed Options
    |> D.andMap (D.field "charlevel" D.int)
    |> D.andMap (D.field "id" D.string)
    |> D.andMap (D.field "display_id" D.string)
    |> D.andMap (D.field "origin" D.string)
    |> D.andMap (D.field "origin_category" D.string)
    |> D.andMap (D.field "display_origin_category" D.string)
    |> D.andMap (D.field "origin_category_index" D.int)
    |> D.andMap extractSpecAndChoice

extractSpecAndChoice : Decoder SpecAndChoice
extractSpecAndChoice =
  D.field "spec" specDec |>
  D.andThen (\spec -> D.field "choice"
               (D.oneOf [ addChoiceDec spec
                        , D.null spec
                        ]))

addChoiceDec : SpecAndChoice -> Decoder SpecAndChoice
addChoiceDec spec = 
  case spec of
    ListSC _ options ->
      D.map (\choice -> ListSC (Just choice) options) D.string
    OrSC _ left right ->
      D.field "choicetype" (Util.matchStringDec "or") |>
      D.andThen (\_ -> D.field "side" dirDec) |>
      D.andThen (addOrChoiceDec left right)
    FromSC unique limit (subspec :: _) ->
      D.map (FromSC unique limit) <|
        D.lazy (\_ -> D.map
                  (\specs ->
                     case limit of
                       Just n -> specs ++ List.repeat (n - List.length specs) subspec
                       Nothing -> specs ++ [subspec])
                  (D.list (addChoiceDec subspec)))
    _ -> D.fail "Page.EditCharacter.addChoiceDec: invalid match"

addOrChoiceDec :  (String, SpecAndChoice) -> (String, SpecAndChoice)
               -> Dir -> Decoder SpecAndChoice
addOrChoiceDec (lname,lspec) (rname,rspec) dir =
  let
    subspec = 
      case dir of
        L -> lspec
        R -> rspec
    choiceDec =
      D.field "choice" (addChoiceDec subspec)  
  in
    D.map
      (\newspec -> case dir of
                     L -> OrSC (Just dir) (lname,newspec) (rname,rspec)
                     R -> OrSC (Just dir) (lname,lspec) (rname,newspec))
      choiceDec

dirDec : Decoder Dir
dirDec =
  D.oneOf [ Util.matchStringDec "left"  |> Util.decSet L
          , Util.matchStringDec "right" |> Util.decSet R
          ]

specDec : Decoder SpecAndChoice
specDec =
  D.field "spectype" D.string |> 
  D.andThen
    (\spectype ->
       case spectype of
         "list"        -> listSpecDec
         "or"          -> orSpecDec
         "from"        -> fromSpecDec False
         "unique_from" -> fromSpecDec True
         _             ->
           D.fail "spectype should be 'list', 'or', 'from', or 'unique_from'")

listSpecDec : Decoder SpecAndChoice
listSpecDec =
  D.field "list" <| D.map (ListSC Nothing) <| D.list
    (D.succeed (\x y -> (x,y))
       |> D.andMap (D.field "opt" D.string)
       |> D.andMap (D.field "desc" (D.oneOf [ D.list D.string
                                            , D.string |> D.map List.singleton
                                            ]
                                   )))
    

orSpecDec : Decoder SpecAndChoice
orSpecDec =
  D.succeed (\lName lSpec rName rSpec -> OrSC Nothing (lName,lSpec) (rName,rSpec))
    |> D.andMap (D.field "leftname" D.string)
    |> D.andMap (D.field "left" (D.lazy (\_ -> specDec)))
    |> D.andMap (D.field "rightname" D.string)
    |> D.andMap (D.field "right" (D.lazy (\_ -> specDec)))

fromSpecDec : Unique -> Decoder SpecAndChoice
fromSpecDec unique =
  D.field "num" (D.oneOf [D.map Just D.int, D.map (\_ -> Nothing) (Util.matchStringDec "unlimited")])
    |> D.andThen
       (\limit ->
          D.map
            (FromSC unique limit)
            (D.field "spec" (D.lazy (\_ ->
                                       case limit of
                                         Just n  -> D.map (List.repeat n) specDec
                                         Nothing -> D.map List.singleton specDec))))
