{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module SerializeableSocialGraph where

import Data.Generics
import HAppS.State


-- Don't import anything from Data.Graph.Inductive.
-- Export whatever you need in UniqueLabelsGraph
-- That way non-unique-label operating functions like mkGraph, insNode, etc are hidden
-- import UniqueLabelsGraph


-- If your datatype is read/showable, you can use a deriving clause to get a serializeable
-- instance, as I do below.
-- However in the fgl, Gr isn't readable, which violates read/show invertability and probably
-- should be patched in the fgl at some point.
-- So I modified code from the fgl in the modules below, basically just to have a derived readable instance
import SerializeableTree 
import SerializeableFiniteMap

import SerializeableUsers

-- There is Template Haskell machinery for deriving Serialize instances of many data types
-- , including Sets and Maps, in HAppS.Data.SerializeTH. 
-- However, there was no machinery for deriving serializeable Graphs
-- So I declared an instance manually
instance Version (Gr a b) where mode = Primitive
instance (Serialize a,Serialize b, Eq a) => Serialize (Gr a b) where
    getCopy = contain ( fmap ( Gr . fmFromList ) safeGet )
    putCopy (Gr m) = contain . safePut . fmToList $ m


data SocialGraph = SocialGraph {                                                            
  socialgraph :: Gr User Float
} deriving (Read,Show,Eq, Typeable,Data) -- 
instance Version SocialGraph
$(deriveSerialize ''SocialGraph) 

