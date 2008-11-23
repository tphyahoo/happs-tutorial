{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, NoMonomorphismRestriction  #-}
module SerializeableUserInfos where
import HAppS.State
import Data.Generics

import qualified MiscMap as M
import qualified Data.ByteString.Char8 as B
import SerializeableJobs

data UserProfile = UserProfile {
  --billing_rate :: String -- eg "" (blank is ok), "$30-$50/hour", "40-50 Euro/hour", "it depends on the project", etc.
  contact :: B.ByteString -- eg, "thomashartman1 at gmail, 917 915 9941"
  -- tell something about yourself. Edited via a text area. should replace newlines with <br> when displayed.
  , blurb :: B.ByteString
  , consultant :: Bool -- this is what actually determines whether the profile will list as a consultant or not
  , avatar :: B.ByteString -- path to an image file
} deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version UserProfile
$(deriveSerialize ''UserProfile) 

--mutators
set_contact = mod_contact . const
mod_contact f (UserProfile contact blurb consultant avatar) = UserProfile (f contact) blurb consultant avatar

set_blurb = mod_blurb . const
mod_blurb f (UserProfile contact blurb consultant avatar) = UserProfile contact (f blurb) consultant avatar

set_consultant = mod_consultant . const
mod_consultant f (UserProfile contact blurb consultant avatar) = UserProfile contact blurb (f consultant) avatar

data UserInfos = UserInfos {   
  password :: B.ByteString
  , userprofile :: UserProfile
  , jobs :: Jobs
} deriving (Show,Read,Ord, Eq, Typeable,Data)
instance Version UserInfos
$(deriveSerialize ''UserInfos) 


-- as a security measure, require that oldpass agrees with real old pass
set_password oldpass newpass (UserInfos pass up jobs) | pass == oldpass = return $ UserInfos newpass up jobs
                                              | otherwise = fail $ "bad old password: " 

-- mod_password f (UserInfos pass up jobs) = UserInfos (f pass) up jobs
set_userprofile = mod_userprofile . const
mod_userprofile f (UserInfos pass up jobs) = UserInfos pass (f up) jobs
-- set_jobs = mod_jobs . const
add_job jobname job =  mod_jobs $ M.insertUqM jobname job
del_job jobname = mod_jobs $ M.deleteM jobname

set_job = mod_job . const
mod_job f jobname = mod_jobs $ M.adjustM jobname f 
mod_jobs mf (UserInfos pass up (Jobs jobs) ) = either (fail . ("mod_jobs: " ++) )
                                                      (\js -> return $ UserInfos pass up (Jobs js) )
                                                      (mf jobs)

