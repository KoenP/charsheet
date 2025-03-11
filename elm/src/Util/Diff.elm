module Util.Diff exposing (..)

import List

import Types exposing (..)

-- Keep only those traits are new or that have changed relative to the old
-- character sheet.
diffTraitCategories :  List NotableTraitCategory
                    -> List NotableTraitCategory
                    -> List NotableTraitCategory
diffTraitCategories old new =
  let go sortedOld sortedNew =
        case ( sortedOld , sortedNew ) of
          ( o :: os , n :: ns) ->
            case compare o.category n.category of
              -- The category occurs in both the old and new sheet,
              -- so we diff the traits themselves.
              EQ -> { category = o.category , traits = diffTraits o.traits n.traits } :: go os ns

              -- Old category does not occur in new sheet, so we drop it.
              LT -> go os (n :: ns)

              -- New category does not occur in old sheet, so we keep it.
              GT -> n :: go (o :: os) ns

          ( _ , ns ) -> ns

  in go (List.sortBy .category old) (List.sortBy .category new)

diffTraits : List Trait -> List Trait -> List Trait
diffTraits old new =
  let go : List Trait -> List Trait -> List Trait
      go sortedOld sortedNew =
        case ( sortedOld , sortedNew ) of
          ( o :: os , n :: ns ) ->
            case compare (o.name , Maybe.withDefault [] o.desc) (n.name , Maybe.withDefault [] n.desc) of
              -- Traits are equal, so we can drop this one (on both sides).
              EQ -> go os ns

              -- Old trait does not occur in list of new traits, so we advance
              -- the list of old traits.
              LT -> go os (n :: ns)

              -- New trait does not occur in list of old traits, so we keep
              -- the new trait.
              GT -> n :: go (o :: os) ns

          ( _ , ns ) -> ns

  in go (List.sortBy .name old) (List.sortBy .name new)

diffSpellcastingSections :  List SpellcastingSection
                         -> List SpellcastingSection
                         -> List SpellcastingSection
diffSpellcastingSections old new =
  let go sortedOld sortedNew =
        case ( sortedOld , sortedNew ) of
          ( o :: os , n :: ns ) ->
            case compare o.origin n.origin of
              EQ -> { n | spells = diffSpells o.spells n.spells } :: go os ns
              LT -> go os (n :: ns)
              GT -> n :: go (o :: os) ns
          ( _ , ns ) -> ns
  in go (List.sortBy .origin old) (List.sortBy .origin new)

diffSpells : List Spell -> List Spell -> List Spell
diffSpells old new =
  let go sortedOld sortedNew =
        case ( sortedOld , sortedNew ) of
          ( o :: os , n :: ns ) ->
            let _ = if o.name == "burning hands" && n.name == o.name
                    then let _ = Debug.log "o" o
                         in Debug.log "n" n
                    else n
            in case compare o.name n.name of
              EQ -> if o /= n then n :: go os ns else go os ns
              LT -> go os (n :: ns)
              GT -> n :: go (o :: os) ns
          ( _ , ns ) -> ns
  in go (List.sortBy .name old) (List.sortBy .name new)

mergeTraitAndSpellCategories :  List NotableTraitCategory -> List SpellcastingSection
                             -> List (Category, List Trait, List Spell)
mergeTraitAndSpellCategories traitCategories spellcastingSections =
  mergeCategories
    (List.map (\{category, traits} -> (category, traits)) traitCategories)
    (List.map (\{origin  , spells} -> (origin  , spells)) spellcastingSections)

mergeCategories :  List (comparable, List a) -> List (comparable, List b)
                -> List (comparable, List a, List b)
mergeCategories left right =
  let go l r =
        case (l, r) of
          ((xcat,xs) :: lrem , (ycat,ys) :: rrem) ->
            case compare xcat ycat of
              EQ -> ( xcat , xs , ys ) :: go lrem rrem
              LT -> ( xcat , xs , [] ) :: go lrem r
              GT -> ( ycat , [] , ys ) :: go l    rrem
          (_  , []) ->
            List.map (\(xcat , xs) -> (xcat , xs , [])) l
          ([] , _ ) ->
            List.map (\(ycat , ys) -> (ycat , [] , ys)) r
  in go (List.sortBy Tuple.first left) (List.sortBy Tuple.first right)
