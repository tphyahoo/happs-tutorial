{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction, ScopedTypeVariables #-}
module SerializeableJobs where

import HAppS.State
import Data.Generics
import qualified MiscMap as M
import qualified Data.ByteString.Char8 as B

-- JobName is like a primary key, Job is like the other fields, if we were in rdbms land

t = let f (JobName (j :: B.ByteString)) = B.unpack j in f . JobName $ (B.pack "job")
-- It might be a bit of overkill to declare things with this level of specificity
-- but I think it'll make the type signatures easier to read later on.
newtype JobName = JobName { unjobname :: B.ByteString }
  deriving (Show,Read,Ord, Eq, Typeable,Data)

instance Version JobName
$(deriveSerialize ''JobName) 

data Job = Job {jobbudget :: B.ByteString -- we allow jobs with unspecified budgets
                , jobblurb :: B.ByteString}
  deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version Job
$(deriveSerialize ''Job) 

-- because Haskell records are a kludge, define mutator functions. bleh. oh well.
-- mod_field takes a mutator function
-- set_field takes a value
set_jobbudget = mod_jobbudget . const
mod_jobbudget f (Job bud blu) = Job (f bud) blu

set_jobblurb = mod_jobblurb . const 
mod_jobblurb f (Job bud blu) = Job bud (f blu)

newtype Jobs = Jobs { unjobs :: M.Map JobName Job }
  deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version Jobs
$(deriveSerialize ''Jobs) 