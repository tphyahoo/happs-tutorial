{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, 
             MultiParamTypeClasses, DeriveDataTypeable, TypeFamilies,
             TypeSynonymInstances, ScopedTypeVariables #-}

module AppStateSetBased where  

import qualified MiscMap as M
-- import qualified Data.Set as S
import Data.Maybe 
import Data.List

import Control.Monad (liftM)
import Control.Monad.Reader (ask)
import Control.Monad.State (modify,put,get,gets)
import Data.Generics
import HAppS.State
import qualified Data.ByteString.Char8 as B
import SerializeableSessions
import SerializeableUsers
import SerializeableUserInfos (UserProfile (..), UserInfos (..), add_job, del_job, set_userprofile, set_job )
import SerializeableJobs (Jobs (..), Job (..), JobName(..) )
import Misc
--import Data.Graph.Inductive

-- Think of appdatastore as the database in a traditional web app.
-- Data there gets stored permanently
-- Data in appsessions is stored permanently too, but we don't care as much about its persistence,
-- it's just to keep track of who is logged in at a point in time.
-- appsessions field could be less complicated, just have M.Map Int SessionData
-- don't really see the advantage of declaring a wrapper over map.

-- to do: appdatastore should be :: Map UserName User
-- User :: Password ConsultantProfile Jobs
-- Jobs :: Map JobName Job
-- Job :: JobBudget JobBlurb
-- thereafter.......... 


data AppState = AppState {
  appsessions :: Sessions SessionData,  
  appdatastore :: Users
} deriving (Show,Read,Typeable,Data)                                                        

instance Version AppState

$(deriveSerialize ''AppState) 

instance Component AppState where 
  type Dependencies AppState = End 
  initialValue = AppState { appsessions = (Sessions M.empty),
                         appdatastore = Users M.empty }


-- myupdate field newval record = record { field = newval }

askDatastore :: Query AppState Users
askDatastore = do
  (s :: AppState ) <- ask
  return . appdatastore $ s


askSessions :: Query AppState (Sessions SessionData)
askSessions = return . appsessions =<< ask

setUserProfile :: UserName -> UserProfile -> Update AppState ()
setUserProfile uname newprofile = modUserInfos uname $ set_userprofile newprofile

-- addJob :: UserName -> JobName -> Job -> Update AppState (Either String ())
addJob uname jn j = modUserInfosM uname $ add_job jn j


-- delJob :: UserName -> JobName -> Update AppState (Either String ())
delJob uname jn = modUserInfosM uname $ del_job jn 


setJob uname jn j = modUserInfosM uname $ set_job j jn

modUserInfosM :: UserName -> (UserInfos -> Either String UserInfos) -> Update AppState (Either String ())
modUserInfosM un mf = do
  (AppState sessions (Users users)) <- get
  case (M.adjustMM un mf users) of
    Left err -> return . Left $ err
    Right um -> do put $ AppState sessions (Users um)
                   return . Right $ ()

modUserInfos :: UserName -> ( UserInfos -> UserInfos ) -> Update AppState ()
modUserInfos un f = do 
  (AppState sessions (Users users)) <- get
  case (M.adjustM un f users) of
    Left err -> fail err
    Right um -> put $ AppState sessions (Users um)





--modify (\s -> (AppState (appsessions s) (f $ appdatastore s)))                              

modSessions :: (Sessions SessionData -> Sessions SessionData) -> Update AppState ()
modSessions f = modify (\s -> (AppState (f $ appsessions s) (appdatastore s)))                           

-- yecchh.
-- the way setmap is being used seems kludgy
-- should probably either be using HAppS IndexSet, or a Map instead of Set.

isUser :: UserName -> Query AppState Bool
isUser name = do
  (Users us ) <- return . appdatastore =<< ask
  if (isJust $ M.lookup name us)
    then return True
    else return False

{-
addUser :: UserName -> B.ByteString -> Update AppState ()
addUser un@(UserName name) hashedpass = do
  AppState s us <- get
  case ( add_user un hashedpass us :: Either String Users) of
    Left err -> fail $ "addUser, name: " ++ (B.unpack name)
    Right newus -> put $ AppState s newus
-}

addUser :: UserName -> B.ByteString -> Update AppState (Either String ())
addUser un@(UserName name) hashedpass = do
  AppState s us <- get
  case ( add_user un hashedpass us :: Either String Users) of
    Left err -> if isInfixOf "duplicate key" err
                  then return . Left $ "username taken"
                  else return . Left $ "error: " ++ err
    Right newus -> do put $ AppState s newus
                      return $ Right ()


changePassword :: UserName -> B.ByteString -> B.ByteString -> Update AppState ()
changePassword un oldpass newpass = do
  AppState s us <- get
  case ( set_user_password un (B.pack hashedoldpass) (B.pack hashednewpass) us :: Either String Users) of
    Left err -> fail $ "changePassword"
    Right newus -> put $ AppState s us
  where hashedoldpass = scramblepass (B.unpack oldpass)
        hashednewpass = scramblepass (B.unpack newpass)


-- was getUser
getUserInfos :: UserName -> Query AppState (Maybe UserInfos)
getUserInfos u = ( return . M.lookup u . users ) =<< askDatastore

getUserProfile u = do
  mbUI <- getUserInfos u
  case mbUI of 
    Nothing -> return Nothing
    Just (UserInfos pass profile jobs) -> return $ Just profile

-- list all jobs along with the username who posted each job
-- listAllJobs :: Query AppState (M.Map UserName Jobs)
listAllJobs = return .
                  concat . M.elems
                    . M.mapWithKey g                 
                       . M.map (unjobs . jobs) . users 
                           =<< askDatastore 
  where g uname jobs = map ( \(jobname,job) -> (jobname,job,uname) ) . M.toList $ jobs


-- lookupUser f users = find f . S.toList $ users
listUsers :: Query AppState [UserName]
listUsers = ( return . M.keys . users ) =<< askDatastore

listUsersWantingDevelopers =  (return . M.keys . M.filter wantingDeveloper . users) =<< askDatastore
  where wantingDeveloper uis = not . M.null . unjobs . jobs $ uis



newSession :: SessionData -> Update AppState SessionKey
newSession u = do  
  AppState (Sessions ss) us <- get
  (newss,k) <- inssess u ss  
  -- check that random session key is really unique
  --modSessions $ Sessions . (M.insert key u) . unsession
  put $ AppState (Sessions newss) us
  return k
  where
    inssess u sessions = do
      key <- getRandom
      case (M.insertUqM key u sessions) of
        Nothing -> inssess u sessions
        Just m -> return (m,key)

delSession :: SessionKey -> Update AppState ()
delSession sk = modSessions $ Sessions . (M.delete sk) . unsession

getSession::SessionKey -> Query AppState (Maybe SessionData)
getSession key = liftM (M.lookup key . unsession) askSessions

numSessions :: Query AppState Int
numSessions  =  liftM (M.size . unsession) askSessions

-- initializeDummyData dd = modUsers (const dd)
initializeDummyData dd = do
  AppState ss (Users us) <- get
  if M.null us 
    then fail "initializeDummyData, users not empty"
    else put $ AppState ss (Users dd)

-- bad performance for large unumbers of users (>1000, with 200 jobs/dummy user)
-- maybe macid doesn't like serializing large quantities of data at once
addDummyData dd = do
  AppState ss (Users us) <- get
  put $ AppState ss (Users (M.union us dd) )

addDummyUser (un,uis) = do
  AppState ss (Users us) <- get
  us' <- M.insertUqM un uis us
  put $ AppState ss (Users us' )


-- define types which are upper case of methods below, eg AddUser, AuthUser...
-- these types work with HApppS query/update machinery
-- in ghci, try :i AddUser
$(mkMethods ''AppState
    ['askDatastore
     , 'getUserInfos
     , 'getUserProfile
     , 'addUser
     , 'changePassword 
     , 'setUserProfile
     -- , 'updateUser
     , 'isUser
     , 'listUsers     
     , 'listAllJobs
     , 'getSession
     , 'newSession
     , 'delSession
     , 'numSessions
     , 'initializeDummyData
     , 'addDummyData
     , 'addDummyUser
     , 'addJob
     , 'delJob
     , 'setJob ]
 )



