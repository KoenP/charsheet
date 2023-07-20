module Page.EditCharacter exposing (..)

import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as E
import Http
import Css exposing (Style)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Platform.Cmd as Cmd
import Dict exposing (Dict)
import Zipper exposing (Zipper(..))

import Request exposing (requestUrl)
import Types exposing (..)
import Util exposing (simple)

--------------------------------------------------------------------------------
-- LOAD
--------------------------------------------------------------------------------
load : Cmd Msg
load =
  Http.get
    { url = requestUrl "options" []
    , expect = Http.expectJson
               (mkHttpResponseMsg GotCharacterOptions)
               (D.list optionsDec)
    }

optionsDec : Decoder Options
optionsDec =
  D.succeed Options
    |> D.andMap (D.field "charlevel" D.int)
    |> D.andMap (D.field "id" D.string)
    |> D.andMap (D.field "origin" D.string)
    |> D.andMap (D.field "origin_category" D.string)
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
      D.map (\choice -> ListSC (Just choice) options) <| D.string
    OrSC _ (leftName, leftSpec) (rightName, rightSpec) ->
      D.field "choicetype" (Util.matchStringDec "or") |>
      D.andThen (\_ -> D.field "side" dirDec) |>
      D.andThen (addOrChoiceDec spec)
    FromSC unique n (subspec :: _) ->
      D.map (FromSC unique n) <|
        D.lazy (\_ -> D.map
                  (\specs -> specs ++ List.repeat (n - List.length specs) subspec)
                  (D.list (addChoiceDec subspec)))
    _ -> D.fail "Page.EditCharacter.addChoiceDec: invalid match"

addOrChoiceDec : SpecAndChoice -> Dir -> Decoder SpecAndChoice
addOrChoiceDec spec dir = D.fail "TODO: addOrChoiceDec"

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
         "list"        -> D.map (ListSC Nothing) (D.field "list" (D.list D.string))
         "or"          -> orSpecDec
         "from"        -> fromSpecDec False
         "unique_from" -> fromSpecDec True
         _             ->
           D.fail "spectype should be 'list', 'or', 'from', or 'unique_from'")

orSpecDec : Decoder SpecAndChoice
orSpecDec =
  D.succeed (\lName lSpec rName rSpec -> OrSC Nothing (lName,lSpec) (rName,rSpec))
    |> D.andMap (D.field "leftname" D.string)
    |> D.andMap (D.field "left" (D.lazy (\_ -> specDec)))
    |> D.andMap (D.field "rightname" D.string)
    |> D.andMap (D.field "right" (D.lazy (\_ -> specDec)))

fromSpecDec : Unique -> Decoder SpecAndChoice
fromSpecDec unique =
  D.field "num" D.int
    |> D.andThen
       (\n ->
          D.map
            (FromSC unique n)
            (D.field "spec" (D.lazy (\_ -> D.map (List.repeat n) specDec))))



--   D.succeed (FromSC unique)
--     |> D.andMap (D.field "num" D.int)
--     |> D.andMap (D.field "spec"
--                    (D.lazy (\_ -> D.map List.singleton specDec)))

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------
update : Msg -> Model -> List Options -> Int -> (Model, Cmd Msg)
update msg model options selectedLevel =
  case msg of
    HttpResponse (Ok (GotCharacterOptions newOptions)) ->
      applyPage model (EditCharacterPage newOptions selectedLevel, Cmd.none)
    HttpResponse (Ok ChoiceRegistered) ->
      (model, load)
    EditCharacterLevel newLevel ->
      applyPage model (EditCharacterPage options newLevel, Cmd.none)
    _ ->
      let
        _ = Debug.log "" msg
      in 
        errorPage model ("Page.EditCharacter.update called with "
                           ++ Debug.toString msg)
      

--------------------------------------------------------------------------------
-- VIEW
--------------------------------------------------------------------------------
view : List Options -> Level -> List (Html Msg)
view opts selectedLevel =
  [ viewSideNav (Debug.log "view" opts), viewMain opts selectedLevel ]
  
viewSideNav : List Options -> Html Msg
viewSideNav opts =
  div
    [ Attr.css sideNavStyle ]
    (opts
    |> List.map .charlevel
    |> List.sort
    |> nubSorted
    |> List.map viewSideNavLevelButton
    )
nubSorted : List a -> List a
nubSorted sortedList =
  case sortedList of
    x :: y :: ys -> 
      if x == y then nubSorted (y :: ys) else x :: nubSorted (y :: ys)
    _            ->
      sortedList

viewSideNavLevelButton : Int -> Html Msg
viewSideNavLevelButton lvl =
  button
    [ Attr.css sideNavButtonStyle
    , E.onClick (EditCharacterLevel lvl)
    ]
    [ text ("Level " ++ String.fromInt lvl) ]


viewMain : List Options -> Level -> Html Msg
viewMain opts selectedLevel =
  div
    [ Attr.css
        [ Css.marginLeft (Css.px 160)
        , Css.padding2 (Css.px 0) (Css.px 10)
        ]
    ]
    (opts
    |> List.filter (\opt -> opt.charlevel == selectedLevel)
    |> Util.multiDictFromList .origin_category
    |> Dict.map viewOriginCategoryOptions
    |> Dict.values)

viewOriginCategoryOptions : String -> List Options -> Html Msg
viewOriginCategoryOptions category optionsList =
  div [] (simple h2 ("From " ++ category ++ ":") :: List.map viewOptions optionsList)

viewOptions : Options -> Html Msg
viewOptions {origin, spec, id} =
  div
    []
    [ simple h3 id
    , viewSpec origin id (Choice origin id << SingletonChoice) spec ]

viewSpec : String -> String -> (String -> Msg) -> SpecAndChoice -> Html Msg
viewSpec origin id mkMsg spec =
  case spec of

    ListSC selected options ->
      viewListSC origin id mkMsg selected options

    FromSC unique n subspecs ->
      let
         choicesList : List String
         choicesList = List.concatMap extractChoicesList subspecs

         editFunctions : List (String -> Msg)
         editFunctions = choiceEditFunctions origin id choicesList
      in 
        div [] <|
          List.map2
             (viewSpec origin id)
             editFunctions
             subspecs
          ++
          List.map
            (viewSpec origin id (\opt -> Choice origin id <| ListChoice <| choicesList ++ [opt]))
            (List.drop (List.length editFunctions) subspecs)

    _ ->
      text "TODO"

viewListSC :  String -> String -> (String -> Msg) -> Maybe String -> List String
           -> Html Msg
viewListSC origin id mkMsg selected options =
  select [ E.onInput mkMsg ] <|
    case selected of
      Nothing ->
        option [Attr.selected True, Attr.disabled True] [text "-- select an option --"]
        :: List.map (viewListSpecOption False) options
      Just selectedVal ->
        option [Attr.disabled True] [text "-- select an option --"]
        :: List.map (\opt -> viewListSpecOption (opt == selectedVal) opt) options
        
viewListSpecOption : Bool -> String -> Html Msg
viewListSpecOption isSelected opt =
  option
  [Attr.value opt, Attr.selected isSelected]
  [text opt]

choiceEditFunctions : String -> String -> List String -> List (String -> Msg)
choiceEditFunctions origin id choices =
  case choices of
    [] ->
      [ Choice origin id << ListChoice << List.singleton ]
    c :: cs ->
      let
        zipper = Zipper [] c cs

        overwriteFocused : Zipper String -> (String -> Msg)
        overwriteFocused (Zipper pre _ post) =
          \x -> Zipper pre x post |> Zipper.toList |> ListChoice |> Choice origin id
      in 
        Zipper.toList (Zipper.extend overwriteFocused zipper)

groupOptionsByOriginCategory : List Options -> Dict String (List Options)
groupOptionsByOriginCategory =
  Util.multiDictFromList .origin_category

--------------------------------------------------------------------------------
-- STYLES
--------------------------------------------------------------------------------

sideNavStyle : List Style
sideNavStyle = 
  [ Css.height (Css.pct 100)
  , Css.width (Css.px 160)
  , Css.position Css.fixed
  , Css.zIndex (Css.int 1)
  , Css.top Css.zero
  , Css.left Css.zero
  , Css.backgroundColor (Css.hex "010101")
  , Css.overflowX Css.hidden
  ]

sideNavButtonStyle : List Style
sideNavButtonStyle =
  [ Css.backgroundColor Css.transparent
  , Css.border Css.zero
  , Css.marginLeft (Css.px 20)
  , Css.marginTop (Css.px 15)
  , Css.padding Css.zero
  , Css.color (Css.hex "818181")
  , Css.cursor Css.pointer
  , Css.fontSize (Css.px 25)
  , Css.display Css.block
  , Css.hover [Css.color (Css.hex "ffffff")]
  ]
