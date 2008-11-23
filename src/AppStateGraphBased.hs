{-# LANGUAGE TemplateHaskell, FlexibleInstances, FlexibleContexts, 
             MultiParamTypeClasses, DeriveDataTypeable, TypeFamilies,
             TypeSynonymInstances, PatternSignatures #-}

module AppStateGraphBased where  

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Control.Monad.Reader
import Control.Monad.State (modify,put,get,gets)

import Data.Generics
import HAppS.State


import SerializeableSessions
import SerializeableSocialGraph
import SerializeableUsers

import Misc
import SerializeableTree

{-
DynGraphUqL is class hack datastructure for a graph with unique labels, with functions
for non-unique-labeled graphs hidden from imported Data.Graph.Inductive

(Eg mkGraph, insNode, etc are hidden)

So don't import anything from Data.Graph.Inductive.
Export whatever you need in UniqueLabelsGraph


class (Ord a, DynGraph gr) => DynGraphUqL a gr where ........
-}
import UniqueLabelsGraph



-- Think of appdatastore as the database in a traditional web app.
-- Data there gets stored permanently
-- Data in appsessions is stored permanently too, but we don't care as much about its persistence,
-- it's just to keep track of who is logged in at a point in time.
-- appsessions field could be less complicated, just have M.Map Int SessionData
-- don't really see the advantage of declaring a wrapper over map.

-- Works! At least, it compiles :)
data AppState = AppState {
  appsessions :: Sessions SessionData,  
  appdatastore :: SocialGraph
} deriving (Show,Read,Typeable,Data)
instance Version AppState                                                                      
$(deriveSerialize ''AppState) 
instance Component AppState where                                                              
  type Dependencies AppState = End                                                             
  initialValue = AppState { appsessions = (Sessions M.empty),
                         appdatastore = SocialGraph empty }

askDatastore = do
  (s :: AppState ) <- ask
  return . appdatastore $ s

askUsersGraph = do
  s <- askDatastore
  return . socialgraph $ s

askUsersSet :: Query AppState (S.Set User)                                    
askUsersSet = do
  g <- askUsersGraph
  return . labelsetFromGraph $ g


askSessions :: Query AppState (Sessions SessionData)
askSessions = return . appsessions =<< ask

-- modUsers :: ( S.Set User -> S.Set User ) -> Update AppState ()
modGraph :: ( Gr User Float -> Gr User Float ) -> Update AppState ()
modGraph f = modify (\appS -> (AppState (appsessions appS)
                                        ( SocialGraph . f . socialgraph . appdatastore $ appS)))

modSessions :: (Sessions SessionData -> Sessions SessionData) -> Update AppState ()
modSessions f = modify (\s -> (AppState (f $ appsessions s) (appdatastore s)))                           

isUser :: String -> Query AppState Bool
isUser name = do
  return . (S.member name) . (setmap username) =<< askUsersSet

-- I tried to declare a functor instance for Set a but got blocked.
setmap f = S.fromList . map f . S.toList


addUser :: String -> String -> Update AppState ()
addUser name pass = modGraph $ addLab (User name pass Nothing Nothing ) 

--modUsers :: ( S.Set User -> S.Set User ) -> Update AppState ()
modUsers f = modify (\s -> (AppState (appsessions s) (f $ appdatastore s)))


changePassword :: String -> String -> String -> Update AppState ()
changePassword username oldpass newpass = modUsers $ changepasswordPure username oldpass newpass


-- yowch! modify is delete plus insert? 
-- this will work for the time being, but will probably eventually need to be something smarter
-- also it would be nice if the type could reflect the possibility of failure, (Maybe, Either, etc)
-- but can't figure out how to do that smartly
-- modify is simply delete plus insert 
changepasswordPure :: String -> String -> String -> SocialGraph -> SocialGraph
changepasswordPure u inputtedOldpass inputtedNewpass (SocialGraph usersgraph ) = 
   let hashedoldpass = scramblepass inputtedOldpass
       hashednewpass = scramblepass inputtedNewpass
       mbU = lookupUser ( (==u). username) usersgraph 
       resetP olduser@(User u realoldpass mbCP mbJobs ) =
           if realoldpass == hashedoldpass
              then modLab (const $ User u hashednewpass mbCP mbJobs ) olduser $ usersgraph
              else usersgraph
   in  SocialGraph $ maybe usersgraph resetP mbU  

getUser :: String -> Query AppState (Maybe User)
getUser u = do
  askUsersGraph >>=
    return . lookupUser ((==u) . username)

lookupUser f users = find f . S.toList . labelsetFromGraph $ users


listUsers :: Query AppState [String]
listUsers = liftM (map username . S.toList) askUsersSet
newSession :: SessionData -> Update AppState SessionKey
newSession u = do
  key <- getRandom
  modSessions $ Sessions . (M.insert key u) . unsession
  return key

delSession :: SessionKey -> Update AppState ()
delSession sk = modSessions $ Sessions . (M.delete sk) . unsession


getSession::SessionKey -> Query AppState (Maybe SessionData)
getSession key = liftM (M.lookup key . unsession) askSessions

numSessions :: Query AppState Int
numSessions  =  liftM (M.size . unsession) askSessions

-- define types which are upper case of methods below, eg AddUser, AuthUser...
-- these types work with HApppS query/update machinery
-- in ghci, try :i AddUser

$(mkMethods ''AppState
    ['askUsersSet
     , 'getUser        
     , 'addUser
     , 'changePassword     
     , 'isUser
     , 'listUsers
     , 'getSession
     , 'newSession
     , 'delSession
     , 'numSessions]
 )

