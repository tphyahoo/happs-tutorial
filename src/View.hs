module View where

import Text.StringTemplate
import Misc
-- use directoryGroupSafer instead,
-- otherwise there are annoyances with punctuation containing emacs backup files and the like
-- import Text.StringTemplate hiding (directoryGroup) 
import qualified Data.Map as M
import Data.List
import HAppS.Server.HTTP.Types (rqURL, Request)
import qualified Data.ByteString.Char8 as B
import Data.Char
import Control.Monad.Reader
import Network.HTTP (urlEncode)
import System.Directory (doesFileExist)
import Data.Maybe
import StateStuff

--import SerializeableUsers 
--import SerializeableUserInfos (UserProfile (..))
--import SerializeableJobs (Job(..), JobName(..))
import HAppS.Helpers.HtmlOutput

import MiscStringTemplate

-- debug problem with foreign character display
-- foreign chars displau okay
--t = do templates <- directoryGroup "templates" 
--       -- let content = renderTemplateGroup templates [] "foreignchars"
--       writeFile "output.html" $ tutlayout (RenderGlobals templates Nothing) [] "foreignchars"


-- Notice, there are no HApps.* imports 
-- Idea is, view is meant to be used from controller.
-- Try to keep functions pure: > :browse View in ghci should reveal there's no IO for any of these function sigs.

data RenderGlobals = RenderGlobals {
                            origrq :: Request
                            , templates :: STGroup String
                            , mbSession :: Maybe SessionData
                            }
  deriving Show

tutlayout :: RenderGlobals -> [([Char], [Char])] -> String -> String
tutlayout (RenderGlobals rq ts mbSess) attrs tmpl0 = 
  let tmpl = cleanTemplateName tmpl0
      mbU = return . sesUser =<< mbSess
      rendertut :: [(String,String)] -> String -> String
      rendertut attrs file = ( renderTemplateGroup ts ) attrs file

      -- should use readM, or whatever it's called, from Data.Safe
      --readtut :: (Monad m, Read a) => String -> m a
      readtut file = (safeRead . rendertut [] . concatMap escapequote $ file)
        where escapequote char = if char=='"' then "\\\"" else [char]
      readTutTuples :: String -> [(String,String)]
      readTutTuples f = either (const [("readTutTuples error","")]) id
                                         $ (readtut f :: Either String [(String,String)] )
      attrsL = maybe attrs( \user -> [("loggedInUser",B.unpack . unusername $ user)] ++ attrs ) mbU  

      content = rendertut attrsL tmpl

      header =  rendertut [("menubarMenu",menubarMenu),("userMenu",userMenu),("mainUserMenu",mainUserMenu)] "header"
        where 
          userMenu = maybe 
            ( rendertut attrsL "login" )
            ( \user -> paintHMenu . map (menuLink rq) $
              [("/tutorial/logout","logout " ++ (B.unpack . unusername $ user))
               , ("/tutorial/changepassword","change password")] )
            ( mbU ) 
          mainUserMenu = if (isJust mbU) 
                           then paintHMenu .
                                  map (menuLink rq)
                                     $ readTutTuples "mainusermenu" 
                           else "" 

          menubarMenu =
            paintHMenu . map (menuLink rq)
              $ readTutTuples "menubarmenu"

      --, ("post-data","form post data")
      --, ("get-data","querystring get data")
      --, ("macid-data","macid data")
      --, ("migrating","changing the data model")
      tocArea = paintVMenuOL . map (menuLink rq ) $ readTutTuples "toc"
           
  
  in rendertut ( [("tocarea",tocArea)
                     , ("contentarea",content)
                     , ("headerarea",header)] ) "base"

cleanTemplateName tmpl = filter isAlpha tmpl
paintblurb b =  newlinesToHtmlLines b

--paintProfile :: RenderGlobals -> String -> UserProfile -> String
paintProfile rglobs user cp userimagepath =
  let attrs = [("username",user) 
               , ("userimage", userimagepath ) -- avatarimage (UserName . B.pack $ user) 
               , ("blurb",newlinesToHtmlLines . B.unpack . blurb $ cp)
               --, ("jobsPosted",paintJobsTable n rglobs $ js)
               , ("contact", newlinesToHtmlLines . B.unpack . contact $ cp)]
  in renderTemplateGroup (templates rglobs) attrs "consultantprofile"



paintUserJobsTable rglobs postedBy rsUserJobs currP resPP = 
  let jobCells = map ( \( JobName j', Job budget blurb)  -> let j :: String
                                                                j = B.unpack j' in 
                       [ joblink postedBy j
                         , simpleLink ("/tutorial/editjob?user="++ (B.unpack postedBy) ++"&job=" ++ j,"edit")
                         , simpleLink ("/tutorial/deletejob?user="++ (B.unpack postedBy) ++"&job=" ++ j,"delete")

                       ] )  rsUserJobs
  in  paintTable Nothing jobCells Nothing -- no pagination for now 

joblink postedBy j = simpleLink ("/tutorial/viewjob?user="++(B.unpack postedBy)++"&job=" ++ j,j)
userlink pBy = simpleLink ("/tutorial/viewprofile?user=" ++ (B.unpack pBy),(B.unpack pBy) )
paintjob rglobs (UserName pBy) (JobName jN, Job jBud jBlu) =
  let 
      joblink = simpleLink ("/tutorial/viewjob?user=" ++ (B.unpack pBy),(B.unpack pBy) )
      attrs = [ ("username",B.unpack pBy)
              , ("jobname",B.unpack jN)               
              , ("budget",B.unpack jBud)
              , ("jobblurb",B.unpack jBlu)
              , ("postedBy",userlink pBy)] 
  in renderTemplateGroup (templates rglobs) attrs "job"

avatarimage un = do
  uap <- urlavatarpath un
  return $ simpleImage (uap,(B.unpack . unusername $ un) ++ " image") ("100","100")

urlavatarpath un = do
  let p = writeavatarpath $ un
  e <- doesFileExist p
  if e
    then return $ "/" ++ p
    else return $ "/static/defaultprofileimage.png"
writeavatarpath un = "userdata/" ++ ( B.unpack . unusername $ un) ++ "/" ++ "profileimage"

