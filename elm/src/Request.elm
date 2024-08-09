module Request exposing (..)

import Url.Builder as Url
import Types exposing (CharId(..))

requestUrl : List String -> List (String, String) -> String
requestUrl path params =
  Url.crossOrigin
    "http://localhost:8000"
    path
    (List.map (\(k,v) -> Url.string k v) params)

characterRequestUrl : CharId -> List String -> List (String, String) -> String
characterRequestUrl (CharId id) suffix params =
  requestUrl (["character", id] ++ suffix) params

-- requestUrl : String -> List (String, String) -> String
-- requestUrl req params =
--   "http://localhost:8000/request/" ++ req
--     ++ case params of
--           [] -> ""
--           ps -> String.concat
--                 ("?" ::
--                    List.intersperse "&" (List.map (\(x,y) -> x ++ "=" ++ y) ps))
