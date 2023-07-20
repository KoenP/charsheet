module Request exposing (..)

import Url.Builder as Url

requestUrl : String -> List (String, String) -> String
requestUrl req params =
  Url.crossOrigin
    "http://localhost:8000"
    ["request", req]
    (List.map (\(k,v) -> Url.string k v) params)

-- requestUrl : String -> List (String, String) -> String
-- requestUrl req params =
--   "http://localhost:8000/request/" ++ req
--     ++ case params of
--           [] -> ""
--           ps -> String.concat
--                 ("?" ::
--                    List.intersperse "&" (List.map (\(x,y) -> x ++ "=" ++ y) ps))
