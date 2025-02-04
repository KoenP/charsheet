module Element.Card exposing (viewSpellCard , viewNotableTraitCard , colorSchemes , defaultColorScheme)

import Css exposing (Color, Style, px, mm)
import Css.Media
import Dict exposing (Dict)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import List
import Markdown
import Platform.Cmd as Cmd
import Set exposing (Set)
import String

import Decoder.CharacterSheet exposing (sheetDec)
import Request exposing (characterRequestUrl)
import Types exposing (..)
import Util exposing (simple, applyIfPresent, guardListLazy)

defaultColorScheme : ColorScheme
defaultColorScheme = { name = "greyscale" , bg = Css.hex "e8e8e8" , fg = Css.hex "000000" }

colorSchemes : List ColorScheme
colorSchemes =
  let mk name bg fg = { name = name , bg = Css.hex bg , fg = Css.hex fg }
  in [ mk "blue" "ddddff" "000096"
     , mk "red" "ffdddd" "960000"
     , mk "green" "ddffdd" "009600"
     , mk "yellow" "eeeedd" "909000"
     , defaultColorScheme
     ]

viewNotableTraitCard : CardConfig -> ColorScheme -> String -> Trait -> Html Msg    
viewNotableTraitCard config ambientColorScheme category { name, desc, ref } =
  let colorScheme = Dict.get ( category , name ) config.traitColorSchemes
        |> Maybe.withDefault ambientColorScheme 
  in div
       [ Attr.css (cardStyle colorScheme) ]
       [ div [ Attr.css (cardTitleSectionStyle colorScheme) ] (viewCardTitle name ref)
       , div [ Attr.css [ Css.flexGrow (Css.num 1), Css.minHeight Css.zero ] ] []
       , div [ Attr.css (descriptionStyle
                           colorScheme
                           (Maybe.withDefault 0
                              (Maybe.map (estimateFontSize << String.length) desc)))
             , Attr.class "card-description"
             ]
           [  Maybe.withDefault "" desc
              |> Markdown.toHtmlWith
                   { githubFlavored = Just { tables = True, breaks = False }
                   , defaultHighlighting = Nothing
                   , sanitize = False
                   , smartypants = True
                   }
                   []
              |> fromUnstyled
           ]

       , div [ Attr.css (cardTypeStyle colorScheme) ] [ text (category ++ " feature") ]
         -- TODO: will show feats as class traits, which is a bit weird
       ]

viewCardTitle : String -> Maybe String -> List (Html Msg)
viewCardTitle title mref =
  applyIfPresent
    mref
    (\ref html -> html ++ [ div
                              [ Attr.css cardSubtitleStyle ]
                              [ text ref ]
                          ])
    [ div [ Attr.css cardTitleStyle ] [ text title ] ]

viewSpellCard : CardConfig -> ColorScheme -> Origin -> Spell -> Html Msg
viewSpellCard cardConfig ambientColorScheme origin spell =
  let colorScheme = Dict.get (origin, spell.name) cardConfig.spellColorSchemes
                    |> Maybe.withDefault ambientColorScheme
  in div [ Attr.css (cardStyle colorScheme) ] <|
       [ div [ Attr.css (cardTitleSectionStyle colorScheme) ]
           [ div [ Attr.css cardTitleStyle ] [text spell.name]
           , div [ Attr.css cardSubtitleStyle ]
                 [ text (cardSubtitle spell
                           ++ Maybe.withDefault ""
                               (Maybe.map (\r -> " · " ++ r) spell.ref)) ]
           ]
       , div [ Attr.css cardBoxesSectionStyle ]
           [ cardBox colorScheme "action-cost-inverted" spell.casting_time
           , cardBox colorScheme "components-inverted" (showComponents spell.components)
           , cardBox colorScheme "rolls-inverted" (Maybe.withDefault "-" spell.rolls)
           , cardBox colorScheme "hourglass-inverted" spell.duration
           , cardBox colorScheme "range-inverted" spell.range
           , cardBox colorScheme "aoe-inverted" (Maybe.withDefault "-" spell.aoe)
           ]
       , div [ Attr.css [ Css.flexGrow (Css.num 1), Css.minHeight Css.zero ] ] []
       , viewSpellDescription colorScheme (getSpellDescriptionText spell) spell.higher_level spell.bonuses spell.resources spell.level
       ]
       ++
       viewConcentrationBadge colorScheme spell.concentration
       ++ 
       [ div [ Attr.css (cardTypeStyle colorScheme) ]
         [ text (origin 
                   ++ case (spell.level, spell.prepared)
                      of (0, _    ) -> " cantrip"
                         (_, False) -> " spell"
                         (_, True ) -> " spell - always prepared")
         ]
       ]

getSpellDescriptionText : Spell -> List String
getSpellDescriptionText spell =
  Maybe.withDefault spell.description <| Maybe.map (\d -> "(Summary:)" :: d) spell.shortdesc

viewSpellDescription : ColorScheme -> List String -> Maybe String -> List SpellBonus -> List PrologTerm -> Int -> Html Msg
viewSpellDescription colorScheme paragraphs higherLevel bonuses resources spellLevel =
  div [ Attr.css (descriptionStyle colorScheme (estimateSpellDescFontSize paragraphs higherLevel bonuses))
      , Attr.class "card-description"
      ]
    <| (  paragraphs
       |> List.intersperse "\n\n"
       |> String.concat
       -- |> Md.toHtml_ Md.Extended 
       |> Markdown.toHtmlWith
            { githubFlavored = Just { tables = True, breaks = False }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = True
            }
            []
       |> fromUnstyled
       )
       -- |> List.map (text >> List.singleton
       --                   >> p [Attr.css [ Css.marginBottom (px 2)
       --                                  , Css.marginTop (px 0)]]))
       ::
       viewHigherLevelP higherLevel
       ++
       viewSpellBonuses bonuses
       ++
       viewSpellResources resources spellLevel

viewConcentrationBadge : ColorScheme -> Bool -> List (Html Msg)
viewConcentrationBadge colorScheme concentration =
  if concentration
  then [ div [Attr.css (concentrationBadgeStyle colorScheme)] [text "concentration"] ]
  else []

viewHigherLevelP : Maybe String -> List (Html Msg)
viewHigherLevelP hl =
  case hl of
    Nothing -> []
    Just hldesc -> [p [] [b [] [text "At higher levels. "], text hldesc]]

viewSpellBonuses : List SpellBonus -> List (Html Msg)
viewSpellBonuses bonuses =
  case bonuses of
    [] -> []
    _  ->
      [ p []
        [ b [] [ text "Bonuses: " ]
        , List.map (formatSpellBonus) bonuses
          |> List.intersperse "; "
          |> String.concat
          |> text
        ]
      ]

formatSpellBonus : SpellBonus -> String
formatSpellBonus { origin, bonus } =
  bonus ++ " (from " ++ origin ++ ")"

viewSpellResources : List PrologTerm -> Int -> List (Html Msg)
viewSpellResources resources spellLevel =
  case (spellLevel, resources) of
    -- Cantrips are always at will, no need to mention it.
    (0, _               ) -> []
    -- If a non-cantrip spell requires no resources, specify that it can be cast at will.
    (_, []              ) -> [ p [] [ b [] [ text "Cast at will." ] ]]
    -- We don't explicitly mention that regular spells need spell slots.
    (_, [Atomic "slot"] ) -> []
    (_, conjunction     ) -> [ p [] ( text "Use "
                                    :: showSpellResourcesList ", " conjunction ++ [text "."] )
                             ]

showSpellResource : PrologTerm -> List (Html Msg)
showSpellResource term =
  case term of
    Atomic "slot"       -> [ b [] [text "spell slot"] ]
    Atomic res          -> [ b [] [text res] ]
    List terms          -> showSpellResourcesList ", " terms
    Compound "per_rest" [Atomic longOrShort, Atomic num]
      -- -> case String.toInt num of
      --      Just n -> List.repeat n viewCheckbox ++ [text <| " / " ++ longOrShort ++ " rest"]
      --      Nothing -> [ text "ERROR (showSpellResource expecting num)" ]
      -> [ b [] [text <| String.concat [num, "x per ", longOrShort, " rest"] ] ]
    Compound "or" terms -> showSpellResourcesList " or " terms
    _                   -> [ text "ERROR" ]

viewCheckbox : Html Msg
viewCheckbox = input [ Attr.type_ "checkbox"
                     , Attr.class "spell-slot"
                     , Attr.css [ Css.width (Css.em 0.8)
                                , Css.height (Css.em 0.8)
                                ] ]
               []

showSpellResourcesList : String -> List PrologTerm -> List (Html Msg)
showSpellResourcesList op resources =
  List.map showSpellResource resources
    |> List.intersperse [text op]
    |> List.concat
  

cardSubtitle : Spell -> String
cardSubtitle spell =
  let levelAndSchool = if spell.level == 0
                       then spell.school ++ " cantrip"
                       else Util.ordinal spell.level ++ " level " ++ spell.school
  in case spell.ritual of
       NotRitual -> levelAndSchool
       Ritual -> levelAndSchool ++ " (ritual)"
       OnlyRitual -> levelAndSchool ++ " (only ritual)"

cardBox : ColorScheme -> String -> String -> Html Msg
cardBox colorScheme iconName value =
  div [ Attr.css (cardBoxStyle colorScheme) ] [ icon colorScheme iconName , text value ]

icon : ColorScheme -> String -> Html Msg
icon colorScheme iconName =
  img
    [ Attr.src ("/static/icons/" ++ iconName ++ ".png")
    , Attr.css [ Css.width (px 10) , Css.marginLeft (mm 1) , Css.backgroundColor colorScheme.fg ]
    ]
    []

showComponents : List Component -> String
showComponents = List.map showComponent >> List.intersperse ", " >> String.concat

showComponent : Component -> String
showComponent c =
  case c of
    V   -> "V"
    S   -> "S"
    M _ -> "M"
                  
----------------------------------------------------------------------
-- STYLE
----------------------------------------------------------------------
cardStyle : ColorScheme -> List Style
cardStyle colScheme =
  [ Css.position Css.relative
  , Css.border3 (px 1) Css.solid (Css.rgb 0 0 0)

  -- MTG size
  , Css.width (mm 63)
  , Css.minWidth (mm 63)
  , Css.height (mm 88)
  , Css.minHeight (mm 88)

  , Css.boxSizing Css.borderBox

  , Css.backgroundColor colScheme.bg |> Css.important
  , Css.borderColor colScheme.fg |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.fontFamilies ["Verdana", "Dosis"]
  , Css.padding (px 4)

  , Css.displayFlex
  , Css.flexDirection Css.column
  ]

cardTitleSectionStyle : ColorScheme -> List Style
cardTitleSectionStyle colScheme =
  [ Css.textAlign Css.center
  , Css.backgroundColor (Css.hex "ffffff") |> Css.important
  , Css.color colScheme.fg |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.borderColor colScheme.fg |> Css.important
  , Css.borderRadius (px 8)
  , Css.border3 (px 2) Css.solid (Css.rgb 0 0 0)
  , Css.displayFlex
  , Css.flexDirection Css.column
  ]

cardTitleStyle : List Style
cardTitleStyle =
  [ Css.fontSize (mm 4.2)
  , Css.textTransform Css.capitalize
  ]

cardSubtitleStyle : List Style
cardSubtitleStyle =
  [ Css.textTransform Css.uppercase
  , Css.fontSize (px 7)
  , Css.fontWeight Css.bold
  ]

cardBoxesSectionStyle : List Style
cardBoxesSectionStyle =
  [ Css.marginTop (mm 1.5)
  , Css.property "display" "grid"
  , Css.property "grid-template-columns" "1fr 1fr 1fr"
  , Css.property "gap" "0.6mm"
  ]

cardBoxStyle : ColorScheme -> List Style
cardBoxStyle colScheme =
  [ Css.backgroundColor (Css.hex "ffffff") |> Css.important
  , Css.color colScheme.fg |> Css.important
  , Css.property "print-color-adjust" "exact"
  , Css.borderRadius (px 6)
  , Css.fontSize (px 8)
  , Css.textAlign Css.center
  , Css.position Css.relative
  , Css.property "display" "grid"
  , Css.property "grid-template-columns" "1fr 5fr"
  , Css.padding (mm 0.7)
  , Css.borderColor colScheme.fg
  , Css.borderStyle Css.solid
  , Css.borderWidth (Css.px 1)
  ]

descriptionStyle : ColorScheme -> Float -> List Style
descriptionStyle colScheme fontSize =
  [ Css.fontSize <| px fontSize
  , Css.lineHeight <| px fontSize
  , Css.textAlign Css.justify
  , Css.backgroundColor (Css.hex "ffffff")
  , Css.color colScheme.fg
  , Css.property "print-color-adjust" "exact"
  , Css.borderRadius (px 8)
  , Css.padding4 Css.zero (mm 1) Css.zero (mm 1)
  , Css.marginBottom (mm 2)
  , Css.borderStyle Css.solid
  , Css.borderWidth (Css.px 2)
  ]

estimateSpellDescFontSize : List String -> Maybe String -> List SpellBonus -> Float
estimateSpellDescFontSize paragraphs higherLevel bonuses =
  if descriptionContainsTable paragraphs
  then 6
  else estimateFontSize (spellDescriptionLength (paragraphs ++ List.map .bonus bonuses) higherLevel)

estimateFontSize : Int -> Float
estimateFontSize len =
  lookupLargestLeq len [(0, 14), (500, 12), (700, 10), (1000, 8), (1400, 7), (1800, 6)]
    |> Maybe.withDefault 6
    -- if len >= 1800
    -- then 6
    -- else if len >= 1600
    --      then 7
    --      else if len >= 1000
    --           then 8
    --           else 10

lookupLargestLeq : comparable -> List (comparable, a) -> Maybe a
lookupLargestLeq x l =
  case l of
    (y, val) :: ys ->
      if x >= y
      then lookupLargestLeq x ys |> Maybe.withDefault val |> Just
      else Nothing
    [] -> Nothing

spellDescriptionLength : List String -> Maybe String -> Int
spellDescriptionLength paragraphs higherLevel =
  List.sum
    <| Maybe.withDefault 0 (Maybe.map String.length higherLevel)
      :: List.map String.length paragraphs


descriptionContainsTable : List String -> Bool
descriptionContainsTable = List.any (String.contains "|---|")


cardTypeStyle : ColorScheme -> List Style
cardTypeStyle colScheme =
  [ Css.position Css.absolute
  , Css.right Css.zero
  , Css.bottom Css.zero
  , Css.fontSize (px 6)
  , Css.textTransform Css.uppercase
  , Css.backgroundColor colScheme.fg
  , Css.color (Css.hex "ffffff")
  , Css.borderRadius4 (mm 1) Css.zero Css.zero Css.zero
  , Css.paddingLeft (mm 0.4)
  , Css.paddingTop (mm 0.4)
  , Css.fontWeight (Css.int 900)
  ]

concentrationBadgeStyle : ColorScheme -> List Style
concentrationBadgeStyle colScheme =
  [ Css.position Css.absolute
  , Css.left Css.zero
  , Css.bottom Css.zero
  , Css.fontSize (px 6)
  , Css.textTransform Css.uppercase
  , Css.backgroundColor colScheme.fg
  , Css.color (Css.hex "ffffff")
  , Css.borderRadius4 Css.zero (mm 1) Css.zero Css.zero
  , Css.paddingRight (mm 0.4)
  , Css.paddingTop (mm 0.4)
  , Css.fontWeight (Css.int 900)
  ]
