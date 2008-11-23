module StateStuff (

  module HAppS.State

  -- If you substitute AppStateGraphBased for AppStateSetBased, 
  -- , move _local ot _local.bak (backup your serialized state)
  -- , and restart app
  -- , your datastore is now graph based
  -- User nodes can connect to each other simply using mkEdge, or whatever else from the fgl.
  -- Now you can clone facebook.
  -- Try doing that with mysql! :)

  , module AppStateSetBased -- AppStateGraphBased -- 

  , module SerializeableUsers
  , module SerializeableSessions
  , module SerializeableUserInfos
  , module SerializeableJobs
{-  , B.ByteString
  , B.unpack 
  , B.pack -}
  ) where
import HAppS.State
import qualified Data.ByteString.Char8 as B
import AppStateSetBased -- AppStateGraphBased -- 
import SerializeableUsers
import SerializeableSessions
import SerializeableUsers
import SerializeableUserInfos
import SerializeableJobs
