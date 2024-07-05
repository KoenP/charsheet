-- To view generated TH:
-- {-# OPTIONS_GHC -ddump-splices #-} 

import           Yesod
import           Yesod.Core
import           Yesod.Static

-- foundation datatype
data Charsheet = Charsheet

instance Yesod Charsheet

getHomeR :: HandlerFor Charsheet ()
getHomeR = do
  sendFile "text/html" "static/index.html"


-- In the "HelloWorld" application: create one route called HomeR ("home resource") and have it listen to GET requests
-- Behavior defined by the getHomeR function
mkYesod "Charsheet" [parseRoutes|
/ HomeR GET
|]
-- /static/charsheet.js JsR GET
-- /static/swipl.js SwiplR GET
-- /static/main.qlf QlfR GET
-- /static/main.pl PlR GET
staticFiles "static/"


-- getJsR :: HandlerFor Charsheet ()
-- getJsR = do
--   sendFile "text/javascript" "static/charsheet.js"
-- 
-- getSwiplR :: HandlerFor Charsheet ()
-- getSwiplR = do
--   sendFile "text/javascript" "static/swipl.js"
-- 
-- getQlfR :: HandlerFor Charsheet ()
-- getQlfR = do
--   sendFile "text/prolog" "static/main.qlf"
-- 
-- getPlR :: HandlerFor Charsheet ()
-- getPlR = do
--   sendFile "text/javascript" "static/main.pl"

main :: IO ()
main = warp 3000 Charsheet
