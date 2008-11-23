{-# LANGUAGE NoMonomorphismRestriction, ScopedTypeVariables #-}

module Controller where

import Control.Monad
import Control.Monad.Trans
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe 
import HAppS.Server
import Text.StringTemplate
import System.FilePath
import System.Directory
import Data.Char


-- state
import StateStuff
import View

import ControllerBasic
import ControllerPostActions
import ControllerGetActions
import ControllerMisc
import ControllerStressTests

import HAppS.Helpers.DirBrowse
import Misc 
import Debug.Trace
--import Data.ByteString (pack,unpack)
import Data.ByteString.Internal 
-- SPs: ServerParts

-- main controller
controller :: Bool -> [ServerPartT IO Response]
controller allowStressTests = {- debugFilter $ -} 
    ( tutorial allowStressTests ) ++ simpleHandlers ++ [ myFavoriteAnimal ] ++ staticfiles 
      ++ [ msgToSp "Quoth this server... 404." ]

tutorial :: Bool -> [ServerPartT IO Response]
tutorial allowStressTests = [ ServerPartT $ \rq -> do
  ts <- liftIO getTemplates
  mbSess <- liftIO $ getmbSession rq
  let mbUName = return . sesUser =<< mbSess
  mbUis <- case mbUName of
           Nothing -> return Nothing
           Just un -> query . GetUserInfos $ un
  unServerPartT ( multi . (tutorialCommon allowStressTests ) $ RenderGlobals rq ts mbSess ) rq
  ] 
  
tutorialCommon :: Bool -> RenderGlobals -> [ServerPartT IO Response]
tutorialCommon allowStressTests rglobs =
   [ exactdir "/" [ ServerPartT $ \_ -> ( return . tutlayoutU rglobs [] ) "home"  ]
     , dir "tutorial" [
           dir "consultants" [ methodSP GET $ viewConsultants rglobs]
         , dir "consultantswanted" [ methodSP GET $ viewConsultantsWanted rglobs ]
         , dir "jobs"   [ methodSP GET $ viewJobs rglobs]
         , dir "logout" [ (logoutPage rglobs)] 
         , dir "changepassword" [ methodSP POST $ changePasswordSP rglobs ]

         , dir "editconsultantprofile" [ methodSP GET $ viewEditConsultantProfile rglobs ]         
         , dir "editconsultantprofile" [ methodSP POST $ processformEditConsultantProfile rglobs ]

         , dir "editjob" [ methodSP GET $ viewEditJobWD rglobs ]
         , dir "deletejob" [ methodSP GET $ deleteJobWD rglobs ]
         , dir "editjob" [ methodSP POST $ processformEditJob rglobs ]

         , dir "postnewjob" [ methodSP POST $ processformNewJob rglobs ]
         , dir "myjobposts" [ methodSP GET $ pageMyJobPosts rglobs ]
         , dir "viewprofile" [ methodSP GET $ userProfile rglobs ]
         , dir "viewjob" [ methodSP GET $ viewJob rglobs ] 
         , dir "actions" $
                 [ dir "login" [ methodSP POST $ loginPage rglobs ]
                   , dir "newuser" [ methodSP POST $ newUserPage rglobs ]
                   -- , dir "upload" [ methodSP POST $ uploadFilePage rglobs ]
                 ]
         , dir "initializedummydata" [ spAddDummyData rglobs ]
         , dir "stresstest"
             [ -- more realistic, higher stress
               dir "atomicinserts" [ spStressTest  allowStressTests ("atomic inserts",atomic_inserts) rglobs] 
               -- faster, insert all users and all jobs in one transaction
               -- fast for small numbers of users, but slow for >1000
               , dir "onebiginsert" [ spStressTest allowStressTests ("one big insert",insertus) rglobs]
               , dir "atomicinsertsalljobs" [ spStressTest allowStressTests ("atomic inserts, all jobs at once",insertusAllJobs) rglobs] 
             ]
         , spJustShowTemplate rglobs
     
   ] ]            

spJustShowTemplate rglobs = lastPathPartSp0 (\_ tmpl -> return $ tutlayoutU rglobs [] tmpl ) 

spStressTest allowStressTest insertf rglobs = 
  if allowStressTest 
    then lastPathPartSp0 $ \_ numusers -> do
         n <- safeRead numusers
         stressTest' insertf n rglobs
    else return $ tutlayoutU rglobs [("errormsg", failmsgStressTest)] "errortemplate"

failmsgStressTest = "<br>-- Stress is blocked from happening on this happs server.\
     \<br>-- For your own stress testinr, run like ./happs-tutorial 5001 True (the second arg controls the stress test)"




staticfiles = [ staticserve "static"
                , staticserve "userdata"
                , browsedirHS "projectroot" "."
                , browsedirHS "templates" "templates"
                , browsedirHS "src" "src" 
              ] 

staticserve d = dir d [ fileServe [] d ]
 
