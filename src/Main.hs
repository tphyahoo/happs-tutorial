-- with gracious thanks to mightybyte:
-- http://softwaresimply.blogspot.com/2008_02_01_archive.html

module Main where
import HAppS.Server
import Controller
import Misc
import System.Environment
import Control.Concurrent
import StateStuff
import System.Time

main = do
  -- let p = 5001 
  let usageMessage = "usage example: happs-tutorial 5001 True (starts the app on port 5001, allows stress tests)"
  args <- getArgs
  case args of
    [port, allowStressTests] -> do
       p <- safeRead port
       allowStressTests <- safeRead allowStressTests
       runserver p allowStressTests
    otherwise -> do
              putStrLn usageMessage
              
-- run the happs server on some port
runserver p allowStressTests = withProgName "happs-tutorial" $ do

  putStrLn . ( "starting happs server" ++ ) =<< time
  control <- startSystemState (Proxy :: Proxy AppState) -- start the HAppS state system

  putStrLn . ( "happs state started" ++ ) =<< time

  tid <- forkIO $ simpleHTTP (Conf {port=p, validator=Nothing}) ( controller allowStressTests )
  putStrLn . ( ( "simpleHttp started on port " ++ (show p) ++ "\n" ++
             "shut down with ctrl-c" ) ++) =<< time

  waitForTermination
  killThread tid
  putStrLn . ( "creating checkpoint" ++ ) =<< time
  createCheckpoint control

  putStrLn .  ( "shutting down system" ++ ) =<< time
  shutdownSystem control 
  putStrLn .  ( "exiting" ++ ) =<< time

     where time = return . ("\ntime: " ++ ) . show  =<< getClockTime


runInGhci = do
    putStrLn $ "happs tutorial running in ghci. \n" ++
             "exit :q ghci completely and reenter ghci, before restarting."
    runserver 5001 True

