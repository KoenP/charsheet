module Decoder.PrologTerm exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D

import Types exposing (..)

prologTermDec : Decoder PrologTerm
prologTermDec =
  D.oneOf
    [ D.map Atomic D.string
    , D.map List (D.list (D.lazy (\_ -> prologTermDec)))
    , D.succeed Compound
        |> D.andMap (D.field "functor" D.string)
        |> D.andMap (D.field "args" (D.list (D.lazy (\_ -> prologTermDec))))
    
    ]
