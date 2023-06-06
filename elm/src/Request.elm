module Request exposing (..)

requestUrl : String -> List (String, String) -> String
requestUrl req params =
  "http://localhost:8000/request/" ++ req
    ++ case params of
          [] -> ""
          ps -> String.concat
                ("?" ::
                   List.intersperse "&" (List.map (\(x,y) -> x ++ "=" ++ y) ps))
