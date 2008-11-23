{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module SerializeableSessions where

import Data.Generics --for deriving Typeable
import HAppS.State -- for deriving Serialize

import SerializeableUsers (UserName (..))

import qualified Data.Map as M




type SessionKey = Integer                                                                   
newtype SessionData = SessionData {                                                            
  sesUser :: UserName
} deriving (Read,Show,Eq,Typeable,Data,Ord)
instance Version SessionData 
$(deriveSerialize ''SessionData)




data Sessions a = Sessions {unsession::M.Map SessionKey a}                                  
  deriving (Read,Show,Eq,Typeable,Data)
instance Version (Sessions a)
$(deriveSerialize ''Sessions)

