module FromDataInstances where

import HAppS.Server
import StateStuff
import Control.Monad
import Control.Monad.Reader
import Safe
import qualified Data.ByteString.Char8 as B
import Misc
import HAppS.Helpers

-- This could be a lot less verbose, and use shorter variable names,
-- but it's the tutorial instructional example for using FromData to deal with forms, so no harm.
data PaginationUrlData = PaginationUrlData { pCurrBar :: Int
                                             , pResultsPerBar :: Int
                                             , pCurrPage :: Int
                                             , pResultsPerPage :: Int }
instance FromData PaginationUrlData where
    fromData =
        let readerCurrbar, readerResultsPerBar, readerCurrpage,readerResultsPerPage
                :: ReaderT ([(String, Input)], [(String, Cookie)]) Maybe Int
            readerCurrpage = safeRead -- convert string to int
                               =<< look "currentpage" `mplus` return "1" -- get string from urlencoded get string
            readerResultsPerPage =  do n <- look "resultsPerPage" `mplus` return "200" 
                                       safeRead n
            readerCurrbar = safeRead =<< look "currentbar" `mplus` return "1"
            readerResultsPerBar = safeRead =<< look "resultsPerBar" `mplus` return "10000"

            readerPaginationUrlData :: ReaderT ([(String,Input)], [(String,Cookie)]) Maybe PaginationUrlData
            readerPaginationUrlData =
              liftM4 PaginationUrlData readerCurrbar readerResultsPerBar readerCurrpage readerResultsPerPage
        in  readerPaginationUrlData    

data JobLookup = JobLookup {postedBy:: UserName, jobName :: JobName}
instance FromData JobLookup where
    fromData = liftM2 JobLookup (do un <- look "user" `mplus` return ""; return $ UserName $ B.pack un)
                                (do jn <- look "job" `mplus` return ""; return . JobName . B.pack $ jn)


data UserNameUrlString = UserNameUrlString {profilename :: UserName}
instance FromData UserNameUrlString where
    fromData = liftM UserNameUrlString ( return . UserName =<< return . B.pack =<< look "user" `mplus` return "")

data UserAuthInfo = UserAuthInfo UserName B.ByteString
instance FromData UserAuthInfo where
    fromData = liftM2 UserAuthInfo ( do un <- (look "username" `mplus` return "") ; return . UserName . B.pack $ un)
                                   (return . B.pack =<< look "password" `mplus` return "")

data ChangeUserInfo = ChangeUserInfo B.ByteString B.ByteString B.ByteString
instance FromData ChangeUserInfo where
    fromData = liftM3 ChangeUserInfo ( return . B.pack =<< (look "oldpass") `mplus` (return "") )
                                     ( return . B.pack =<< look "password" `mplus` return "")
                                     ( return . B.pack =<< look "password2" `mplus` return "")



-- wrapper over UserProfile data type, the difference is that the avatar is file contents
data EditUserProfileFormData = EditUserProfileFormData 
  B.ByteString -- contact, eg, "thomashartman1 at gmail, 917 915 9941"
  B.ByteString -- blurb
  Bool -- isConsultant, Bool 
  B.ByteString -- avatarcontents, filename

instance FromData EditUserProfileFormData where
    fromData = liftM4 EditUserProfileFormData
                                  ( return . B.pack =<< look "contact" `mplus` return "" )
                                  ( return . B.pack =<< look "consultantblurb" `mplus` return "")
                                  ( readcheckbox "listasconsultant")  
                                  ( return . B.pack =<< look "imagecontents" `mplus` return "") 

data EditJob = EditJob JobName String String
-- Recommendation: ALWAYS give a default value via mplus, otherwise debugging can be hell.
instance FromData EditJob where
    fromData = liftM3 EditJob (do ojn <- look "jobtitle" `mplus` return ""; return . JobName $ B.pack ojn)
                          (look "jobbudget" `mplus` return "")
                          (look "jobdescription" `mplus` return "")

data NewJobFormData = NewJobFormData JobName Job
instance FromData NewJobFormData where
  fromData = liftM2 NewJobFormData lookjobname lookjob
lookjobname = do jn <- (look "jobtitle" `mplus` return "")
                 return . JobName . B.pack $ jn
lookjob = do bud <- return . B.pack =<< (look "jobbudget" `mplus` return "")
             blurb <- return . B.pack =<< (look "jobdescription" `mplus` return "")
             return $ Job bud blurb

data NewUserInfo = NewUserInfo UserName B.ByteString B.ByteString
instance FromData NewUserInfo where
    fromData = liftM3 NewUserInfo (do un <- look "username" `mplus` return ""; return (UserName . B.pack $ un) )
                                  (return . B.pack =<< look "password" `mplus` return "")
                                  (return . B.pack =<< look "password2" `mplus` return "")
