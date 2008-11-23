{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module UniqueLabelsGraph (
  insUqNode, insUqNodes, mkUqGraph, lnodesetFromGraph, labelsetFromGraph, empty, matchLabel, labelExists,
  addLab, modLab,
  DynGraphUqL
)

where

import Data.Graph.Inductive
import Control.Monad.State
import Data.Maybe
import qualified Data.Set as S
import Data.List (foldl',find)

-- like insNode(s) from Data.Graph.Inductive, but only works if node label is unique
-- a class for graphs with unique labels
-- you get an error if you try to insert duplicate labels
-- and there's a function for returning all node labels as a set.
-- wouldn't Ord constraint be better?

-- minimum definition: insUqNode
class (Ord a, DynGraph gr) => DynGraphUqL a gr where
  insUqNode :: LNode a -> gr a b -> gr a b
  insUqNode (v,l) gr | found = error "insNode, duplicate Node " 
                     | otherwise = insNode (v,l) gr
    where found = isJust . find (\(_,lab) -> lab==l) . labNodes $ gr
  matchLabel :: a -> gr a b -> (Maybe (Data.Graph.Inductive.Context a b), gr a b)
  matchLabel l g = 
    case filter ( (==l) . snd) . labNodes $ g of
      [] -> (Nothing,g)
      [(nodeI,_)] -> match nodeI g
      otherwise -> error "matchLabel, duplidate labels"

instance (Ord a, DynGraph gr) => DynGraphUqL a gr

insUqNodes vs g = foldr insUqNode g vs

lnodesetFromGraph g = S.fromAscList . labNodes $ g

labelsetFromGraph g = (S.map snd) . lnodesetFromGraph  $ g

mkUqGraph vs es   = (insEdges' . insUqNodes vs) empty
      where
        insEdges' g = foldl' (flip insEdge) g es

 
labelExists l g = let (mcontext,gr) = matchLabel l g
                  in if (isJust mcontext)
                       then True
                       else False

{-
lookupLabel l g = let (mcontext,gr) = matchLabel l g
                   in if (isJust mcontext)
                       then (Ju
                       else False                                     
-}  
  
-- these fail because of duplicate nodes, which is the right thing.
t, t1 :: Gr String Float
t = mkUqGraph [(1,""), (0,"")] [] 
t1 = insUqNode (1,"") . insUqNode (0, "") $ empty



addLab :: (DynGraph g, Ord a) => a -> g a b -> g a b
addLab a g = run_ g $ insMapNodeM a
             
-- fails, as it should, because dupe
t2 :: Gr String ()
t2 = addLab "" . addLab "" $ empty


modLab :: (Ord a, DynGraph g) => (a -> a) -> a -> g a b -> g a b
modLab f nl g = run_ g $ do
  oldgraph <- return . snd =<< get -- couldn't we just say: g instead of oldgraph and skip this?
  let ( mbOldContext, oldRemainingGr ) =  matchLabel nl $ oldgraph
      mbNewGraph = do (fr, nodeId, nodeLabel, to) <- ( mbOldContext )
                      return  $ (fr, nodeId, f nodeLabel, to) & oldRemainingGr
  return $ maybe oldgraph id mbNewGraph 
