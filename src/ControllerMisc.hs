module ControllerMisc where

import HAppS.Server

import Misc
import MiscStringTemplate
import View
import StateStuff
import Text.StringTemplate
import SerializeableJobs
import SerializeableUsers
import Control.Monad
import Control.Monad.Error
import HAppS.Helpers
import qualified Data.ByteString.Char8 as B

-- import SerializeableUsers (User(..))
-- The final value is HtmlString so that the HAppS machinery does the right thing with toMessage.
--   If the final value was left as a String, the page would display as text, not html.
--tutlayoutReq :: Request -> [([Char], String)] -> String -> WebT IO Response
tutlayoutU rglobs attrs tmpl = ( toResponse . HtmlString . tutlayout rglobs attrs ) tmpl

{-
getmbLoggedInUser :: Request -> IO (Maybe UserName)
getmbLoggedInUser rq = do
  mbSd <- maybe ( return Nothing )
            ( query . GetSession )
            ( getMbSessKey rq )
  return $ do
    sd <- mbSd
    return . sesUser $ sd
  where 
-}
getmbSession :: Request -> IO (Maybe SessionData)
getmbSession rq = maybe ( return Nothing ) ( query . GetSession ) ( getMbSessKey rq )


-- to do: make it so keeps your current page if you login/logout
-- probably modify RenderGlobals to keep track of that bit of state
--startsess' :: RenderGlobals -> UserName -> WebT IO Response

-- The user argument could be bytestring rather than say, UserName, to keep things generic
-- This way we could have different types of users, say UserName users and AdminUserName users.
-- But for now, keep UserName.
startsess' ::
  --(HAppS.Server.SURI.ToSURI uri) =>
  (Request -> String) -> RenderGlobals -> UserName -> WebT IO Response
startsess' getLandingpage (RenderGlobals origRq ts _) user = do
  let sd = SessionData user
  key <- update $ NewSession sd
  addCookie (3600) (mkCookie "sid" (show key))
  let newRGlobs = RenderGlobals origRq ts (Just sd) 
  let lp = getLandingpage origRq
  redirectToUrl lp 

getLoggedInUserInfos :: RenderGlobals -> ErrorT String (WebT IO) (UserName,UserInfos)
getLoggedInUserInfos (RenderGlobals _ _ Nothing) = fail "getLoggedInUserInfos, not logged in"
getLoggedInUserInfos (RenderGlobals _ _ (Just (SessionData uN))) = do
  loggedInUserInfos <- ErrorT . return .
                               maybe (Left $ "bad user" ++ (B.unpack . unusername $ uN)) Right
                                 =<< (query $ GetUserInfos uN)
  return (uN,loggedInUserInfos)


-- getMbSessKey rq = readData (readCookieValue "sid") rq
getMbSessKey :: Request -> Maybe SessionKey
getMbSessKey rq = readData (readCookieValue "sid") rq

-- updateUserSp :: RenderGlobals -> UserName -> (RenderGlobals -> ServerPartT IO Response) -> Request -> WebT IO Response
-- take the current user and replace him with newuser. 
-- and what to do in the sp after the update.
-- ugh! rewrite!
-- updateUserSp rglobs newuser withrgSp rq = do
{-
updateUserSp rglobs action withrgSp rq = do
  case mbUser rglobs of
      Nothing -> return $ tutlayoutU rglobs [("errormsg", "updateUserSp: no user")] "errortemplate"
      Just uname -> do
        mbUis <- query $ GetUser uname
        case mbUis of
          Nothing -> return $ tutlayoutU rglobs [("errormsg", "updateUserSp: no user infos")] "errortemplate"
          Just uis -> 
 updateuser olduser newuser
                         let newrglobs = RenderGlobals (templates rglobs) (Just newuser)
                         unServerPartT ( withrgSp newrglobs ) rq 
-}


updateuser olduser newuser = do
  --update (UpdateUser olduser newuser)
  undefined
  return newuser


-- IO action: templates are loaded from templates dir for every handle.
-- Alternative is to read templates once at application start time,
-- but then if you make changes in the templates dir they won't be reflected unless you stop/start the server
-- Option b might be appropriate for situations where there is high traffic and changes to templates rarely happen
-- and it's ok to stop/start server if necessary.
getTemplates :: IO (STGroup String)
getTemplates = directoryGroupSafer "templates"


--very, VERY hackish way of reading a checkbox
readcheckbox :: String -> RqData Bool
readcheckbox checkboxname = return . not . (=="noval") =<< look checkboxname `mplus` return "noval"

logoutPage :: RenderGlobals -> ServerPartT IO Response
logoutPage rglobs@(RenderGlobals origRq ts mbU) =
  withRequest $ \rq -> do
    newRGlobs <-
      maybe
         ( return rglobs )
         ( \sk -> do update . DelSession $ sk
                     return (RenderGlobals origRq ts Nothing)
         )
         (getMbSessKey rq)
    ( return . tutlayoutU newRGlobs [] ) "home"