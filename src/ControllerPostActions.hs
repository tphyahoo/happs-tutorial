{-# LANGUAGE ScopedTypeVariables #-}
module ControllerPostActions where

import Control.Monad
import Control.Monad.Trans
import Data.List (isInfixOf)
import qualified Data.ByteString.Char8 as B
import Control.Monad.Error


import System.FilePath (takeFileName)
import HAppS.Server
import HAppS.Helpers

import StateStuff
import View
import Misc
import ControllerMisc
import ControllerGetActions
import FromDataInstances

loginPage = loginPage' authUser (startsess' getLoginReferer)
  where authUser = authUser' getUserPassword
        getUserPassword name = return . maybe Nothing (Just . B.unpack . password)
                                 =<< query (GetUserInfos name)
        -- after login, just redirect to the last page we were on.
        
        -- try to return the referral, if can't parse it from headers, return home page
        getLoginReferer :: Request -> String
        getLoginReferer rq = 
          let homepage = getAppUrl "" rq
              etRef = getHeaderVal "referer" rq
          in  case etRef of 
                Left _ -> homepage
                Right rf -> if isInfixOf "logout" rf
                  then homepage
                  else rf

loginPage' :: (UserName -> B.ByteString -> WebT IO Bool)
                -> (RenderGlobals -> UserName -> WebT IO Response) 
              -> RenderGlobals -> ServerPartT IO Response
loginPage' auth sessW' rglobs =
  withData $ \(UserAuthInfo user pass) -> 
    [ ServerPartT $ \_ -> do
        loginOk <- auth user pass
        if loginOk
          then sessW' rglobs user
          else errW "login error: invalid username or password"
    ]             
  where errW msg = return $ tutlayoutU rglobs [("loginerrormsg",msg)] "home"

{-
-- check if a username and password is valid. If it is, return the user as successful monadic value
-- otherwise fail monadically
authUser :: Monad m => UserName -> B.ByteString -> WebT IO (m UserInfos)
authUser name pass = do
  mbUser <- query (GetUserInfos name)
  case mbUser of
    Nothing -> return . fail $ "login failed"
    (Just u) -> do
       p <- return . password $ u
       -- scramblepass works with lazy bytestrings, maybe that's by design. meh, leave it for now
       if p == scramblepass pass 
         then return . return $ u
         else return . fail $ "login failed"
-}

-- to do: make it so keeps your current page if you login/logout
-- probably modify RenderGlobals to keep track of that bit of state
{-
startsess :: RenderGlobals -> UserName -> WebT IO Response
startsess (RenderGlobals ts _) user = do
  key <- update $ NewSession (SessionData user)
  addCookie (3600) (mkCookie "sid" (show key))
  let newRGlobs = RenderGlobals ts (Just user) 
  (return . tutlayoutU newRGlobs [] ) "home"
-}


-- check if a username and password is valid. If it is, return the user as successful monadic value
-- otherwise fail monadically
authUser' :: (UserName -> WebT IO (Maybe String) ) -> UserName -> B.ByteString -> WebT IO Bool
authUser' getpwd name pass = do
  mbP <- getpwd name
  -- scramblepass works with lazy bytestrings, maybe that's by design. meh, leave it for now
  -- to do: we need to use a seed, there was a discussion about this on haskell cafe.
  return $ maybe False ( == scramblepass (B.unpack pass) ) mbP


{-
changePasswordSP rglobs = withData $ \(ChangeUserInfo oldpass newpass1 newpass2) -> [ ServerPartT $ \rq -> do
    if newpass1 == newpass2 
       then do mbL <- liftIO $ getmbLoggedInUser rq
               maybe
                 (errW "Not logged in" rq)
                 (\u -> do mbUis <- query (GetUserInfos u)
                           case mbUis of
                             Nothing -> errW ("bad username: " ++ (B.unpack . unusername $ u)) rq 
                             Just uis  -> do update $ ChangePassword u oldpass newpass1
                                             return $ tutlayoutU rglobs [] "accountsettings-changed" )
                 (mbL :: Maybe UserName)
       else errW "new passwords did not match" rq
  ]
  where errW msg rq = ( return . tutlayoutU rglobs [("errormsgAccountSettings", msg)] ) "changepassword" 
-}
changePasswordSP rglobs = withData $ \(ChangeUserInfo oldpass newpass1 newpass2) -> [ ServerPartT $ \rq -> do
    etRes <- runErrorT $ getLoggedInUserInfos rglobs
    case etRes of
      Left e -> errW e
      Right (u,_) -> do
        if newpass1 /= newpass2
           then errW "new passwords did not match"
           else do update $ ChangePassword u oldpass newpass1
                   return $ tutlayoutU rglobs [] "accountsettings-changed"
  ]
  where errW msg = ( return . tutlayoutU rglobs [("errormsgAccountSettings", msg)] ) "changepassword"



processformEditConsultantProfile rglobs =
  withData $ \fd@(EditUserProfileFormData fdContact fdBlurb fdlistAsC fdimagecontents) -> [ ServerPartT $ \rq -> do
 case (return . sesUser =<< mbSession rglobs) of
   Nothing -> errW "Not logged in"
   Just unB -> do
     mbUP <- query $ GetUserProfile unB
     case mbUP of
       Nothing -> errW "error retrieving user infos"
       Just (UserProfile pContact pBlurb listasC pAvatar) -> do
         up <- if B.null (fdimagecontents)
           then return $ UserProfile fdContact fdBlurb fdlistAsC pAvatar
           else do
             let avatarpath = writeavatarpath unB
             -- to do: verify this handles errors, eg try writing to someplace we don't have permission,
             -- or a filename with spaces, whatever
             liftIO $ writeFileForce avatarpath fdimagecontents
             return $ UserProfile fdContact fdBlurb fdlistAsC (B.pack avatarpath)             
         update $ SetUserProfile unB up
         unServerPartT ( viewEditConsultantProfile rglobs) rq
  ]
  where errW msg = ( return . tutlayoutU rglobs [("errormsg", msg)] ) "errortemplate"

processformEditJob :: RenderGlobals -> ServerPartT IO Response
processformEditJob rglobs@(RenderGlobals rq ts mbSess) =
  withData $ \(EditJob jn jbud jblu) -> [ ServerPartT $ \rq -> do
  case (return . sesUser =<< mbSess) of
    Nothing -> errW "Not logged in" 
    -- Just olduser@(User uname p cp js) -> do
    Just uname -> do
      if null (B.unpack . unjobname $ jn)
        then errW "error, blank job name"
        else do 
          update $ SetJob uname jn (Job (B.pack jbud) (B.pack jblu))
          unServerPartT ( viewEditJob uname jn rglobs) rq 
   ]
   where errW msg = ( return . tutlayoutU rglobs [("errormsg", msg)] ) "errortemplate"
        
      
processformNewJob rglobs@(RenderGlobals rq ts mbSess) =
  withData $ \(NewJobFormData jn newjob) -> [ ServerPartT $ \rq -> do
 case (return . sesUser =<< mbSess) of
   Nothing -> errW "Not logged in"
   Just user -> do
     if null (B.unpack . unjobname $ jn)
        then errW "error, blank job name"
        else do 
         res <- update (AddJob user jn newjob)
         case res of 
           Left err -> case isInfixOf "duplicate key" (lc err) of
                              True -> errW "duplicate job name"
                              otherwise -> errW "error inserting job"
           Right () -> unServerPartT ( pageMyJobPosts rglobs ) rq 
  ]
  where errW msg = ( return . tutlayoutU rglobs [("errormsg", msg)] ) "errortemplate"

{-
newUserPage :: RenderGlobals -> ServerPartT IO Response
newUserPage rglobs =
  withData $ \(NewUserInfo user pass1 pass2) -> 
    [ ServerPartT $ \_ ->
         do userExists <- query $ IsUser user
            newuser user pass1 pass2 userExists
    ]
    where errW msg = ( return . tutlayoutU rglobs [("errormsgRegister", msg)] ) "register" 
          newuser :: UserName -> B.ByteString -> B.ByteString -> Bool -> WebT IO Response
          newuser u@(UserName us) pass1 pass2 userExists
              | pass1 /= pass2 = errW "passwords did not match"
              | null . B.unpack $ pass1 = errW "bad password"
              | null . B.unpack $ us = errW "bad username"
              | userExists = errW "User already exists"
              | otherwise =  do update $ AddUser u $ scramblepass pass1
                                mbUis <- query $ GetUserInfos u
                                case mbUis of
                                  Nothing -> errW "newUserPage, update failed"
                                  Just uis -> startsess rglobs u
-}

newUserPage :: RenderGlobals -> ServerPartT IO Response
newUserPage rglobs =
  withData $ \(NewUserInfo user (pass1 :: B.ByteString) pass2) ->
    [ ServerPartT $ \rq -> do
  etRes <- runErrorT $ do
    setupNewUser (NewUserInfo user (pass1 :: B.ByteString) pass2) 
  case etRes of
    Left err -> return $ tutlayoutU rglobs [("errormsgRegister", err)] "register"
    Right () -> startsess' (getAppUrl "tutorial/registered") rglobs user
        ]
  where
    setupNewUser :: NewUserInfo -> ErrorT [Char] (WebT IO) ()
    setupNewUser (NewUserInfo user (pass1 :: B.ByteString) pass2) = do
        if B.null pass1 || B.null pass2
          then throwError "blank password"
          else return ()
        if pass1 /= pass2
          -- TITS: can return . Left be replaced with throwError?
          --  A: no. But you can return just plain Left with throwError.
          then throwError "passwords don't match"
          else return ()
        nameTakenHAppSState <- query $ IsUser user
        if nameTakenHAppSState
          then throwError "name taken"
          else return ()
        addUserVerifiedPass user pass1 pass2

addUserVerifiedPass :: UserName -> B.ByteString -> B.ByteString -> ErrorT String (WebT IO) ()
addUserVerifiedPass user pass1 pass2 = do
  ErrorT $ newuser user pass1 pass2
  where
    newuser :: UserName -> B.ByteString -> B.ByteString -> WebT IO (Either String ())
    newuser u@(UserName us) pass1 pass2 -- userExists
      | pass1 /= pass2 = return . Left $  "passwords did not match"
      | otherwise = update $ AddUser u $ B.pack $ scramblepass (B.unpack pass1)

