{-# LANGUAGE NoMonomorphismRestriction #-}
module ControllerStressTests where

import HAppS.Server
import Data.List
import Misc
import qualified MiscMap as M 
import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Char8 as B

import StateStuff
import ControllerMisc
import System.IO
import View
import System.Time

-- insert a lot of data unrealistically
-- the datastore set gets set in one huge macid transaction
spAddDummyData rglobs = 
  withRequest $ \_ -> do
    us <- query AskDatastore
    if M.null (users us)
       then do insertus ddDemo
               return $ tutlayoutU rglobs [] "dummydatainitialized"   
       else return $ tutlayoutU rglobs [("errormsg", failmsg)] "errortemplate"
  where failmsg = "initializeDummyData, for safety, only works if there is currently no data in app\
                   \Maybe you shouldd first do mv _local _local.bak to get any existing data out of the way." 

-- insert a lot of data, faster, but less realistically -- all the users are inserted at once
--stressTestOneBigInsert = stressTest' insertus

-- insert a lot of data more realistically
-- lots of little macid actions build up to the final datastore.
-- stressTest :: Int -> RenderGlobals -> WebT IO Response
--stressTest = stressTest' $ atomic_inserts
stressTest' (fname,f) n rglobs = do
    startTime <- liftIO $ getClockTime
    (userRange,us) <- getUsers 10 n
    f us
    stressTestTime <- liftIO $ return . timeDiffToString =<< timeSince startTime
    liftIO $ putStrLn $ fname ++ " stresstest, " ++ (show n) ++ " users, elapsedtime: " ++ stressTestTime
    return $ tutlayoutU rglobs [("first",show . head $ userRange)
                                , ("last",show . last $ userRange)
                                , ("stressTestName",fname)
                                , ("testTime",stressTestTime)]
                                  "stresstestcompleted"


--getUsers :: (MonadIO m) => Int -> m ([Int], Users)
getUsers jobsPerU n = do
        us <- return . M.toList . users =<< query AskDatastore
        -- something around here seems kinda slow
        let startNum = case ( mapMaybe ( getDummyNumber . B.unpack . unusername . fst ) us ) of
               [] -> 1
               xs -> (+1) . last . sort $ xs 
            userRange = [startNum..(startNum+n-1)]            
        return $ (userRange, (stresstestdata jobsPerU) userRange)


getDummyNumber :: String -> Maybe Int
getDummyNumber s = do
  let i = parse parseDummyNumber "parseDummyNumber" s
  case i of
    Left err -> Nothing
    Right i -> Just i



parseDummyNumber :: Parser Int
parseDummyNumber = do
  string "user"
  i <- safeRead =<< many digit
  eof
  return i

-- insert a whole mess of users -- less realistic, but maybe faster way
-- to see how website stands up with a large amount of macid data (does it work with 100000 users... let's see...)

insertus :: (MonadIO m) => Users -> m ()
insertus us = update . ( AddDummyData . users ) $ us

--insertuAllJobs :: (MonadIO m) => (UserName, UserInfos) -> Users -> m ()
insertusAllJobs = mapM_ insertuAllJobs . M.toList . users 
insertuAllJobs (u, uis) = do
  update $ ( (AddDummyUser (u,uis)) ) 
  liftIO $ putStrLn $ "insertuAllJobs, added user: " ++ ( B.unpack . unusername $ u)
-- insert a user realistically 
-- follow the same macid steps that would occur when data is added by a human
atomic_inserts = mapM_ insertu . M.toList . users
insertu :: (MonadIO m) => (UserName, UserInfos) -> m ()
insertu (u, UserInfos pass pr (Jobs js) ) = do
  update $ AddUser u (B.pack . scramblepass . B.unpack $ pass)
  update $ SetUserProfile u pr
  sequence_ $ map (\(jn,j) -> update $ AddJob u jn j) ( M.toList js )
  liftIO $ putStrLn $ "insertu, added user: " ++ ( B.unpack . unusername $ u)
    where addj u j = do
            -- liftIO $ putStrLn $ show ("adding job",username u,jobname j)
            addJob u j

                    
--------------dummy data
-- create arbitrarily large numbers of users to attempt insertion
--stresstestdata :: (Integral a) => [a] -> Users
stresstestdata jobsperU users = Users $ M.fromList $ map (stresstestUser jobsperU) users
stresstestUser jobsperU i = ( UserName . B.pack $ ("user"++(show i))  
                     , UserInfos 
                        ( B.pack $ "password" ++ (show i))
                        (stresstestprofile i)
                        (Jobs $ M.fromList $ map (stresstestjob i) [1..jobsperU]) )
  where stresstestprofile i = UserProfile { contact = B.pack $ "someone" ++ (show i) ++ "@somewhere.com"
                                                  , blurb = B.pack $ "la la la"
                                                  , consultant = if even i then True else False
                                                  , avatar = B.pack ""}
        stresstestjob i j = ( JobName . B.pack $ "make something " ++ (show (i,j)) ,
                              Job { jobbudget = B.pack . show $ (i,j)
                                  , jobblurb = B.pack . ("blurb " ++) . show $ (i,j) } ) 
                            

-- dummy data appropriate for a job site demo and testing
-- tphyahoo is consultant type user (wants to find work)
-- zzz posted a lot of test jobs
ddDemo :: Users
ddDemo = Users $ M.fromList [
  ( UserName . B.pack $ "tphyahoo", UserInfos (B.pack . scramblepass $ "password") tphyahooProfile tphyahooJobs )
  , ( UserName . B.pack $ "zzz", UserInfos (B.pack . scramblepass $ "password")
                                         (UserProfile ( B.pack "") ( B.pack "") False ( B.pack "")) serpinskiJobs )
  ] 

tphyahooProfile = UserProfile {
  --billing_rate = "it depends on the project"
  contact = B.pack $ "thomashartman1 at gmail, +48 51 365 3957"
  -- tell something about yourself. Edited via a text area. should replace newlines with <br> when displayed.
  , blurb = B.pack $ "I'm currently living in poland, doing a software sabbatical where I'm \
             \learning new things and writing and releasing open source software, including this tutorial."
  , consultant = True 
  , avatar = B.pack ""
}


-- can't use dummy data with foreign chars
-- app seems to accept them when entered through form though.
--               , ("Umläute in Happs benutzen können", "umsonst?")
tphyahooJobs =
  Jobs $ M.fromList $ map (\(j,b)-> ( JobName . B.pack $ j, Job { jobbudget = B.pack b, jobblurb = B.pack $ "make " ++ j ++ " using HAppS "} ) ) $
             [ ("darcshub", "$5000")
              , ("community wizard", "$500,000")
              , ("hpaste in happs", "karma points?")
              , ("facebook clone", "$10,000")
              , ("rentacoder clone", "12,000 Eu")
              , ("ebay clone", "")
              , ("reddit clone", "")
              , ("ripplepay clone", "best offer")
              , ("oscommerce clone", "$1500")
              , ("phpbb clone", "")
              , ("sql-ledger clone", "")]
serpinskiJobs =  Jobs $ M.fromList $ map (\num -> ( JobName . B.pack $ "job" ++ (show num), Job ( B.pack "$0") ( B.pack "")) ) [10..203]

