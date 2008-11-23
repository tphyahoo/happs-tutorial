-- | Example Graphs
-- snip from Data.Graph.Inductive.Example
module Example where

import Data.Graph.Inductive.Graph 
import SerializeableTree

-- | generate list of unlabeled nodes
genUNodes :: Int -> [UNode]
genUNodes n = zip [1..n] (repeat ())

-- | generate list of labeled nodes
genLNodes :: Enum a => a -> Int -> [LNode a]
genLNodes q i = take i (zip [1..] [q..])

-- | denote unlabeled edges
labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))

-- | empty (unlabeled) edge list
noEdges :: [UEdge]
noEdges = [] 


a   :: Gr Char ()
a    = ([],1,'a',[]) & empty                  -- just a node

-- readable, showable, serializeable graphs.
t2 = putStrLn . show $ tgr
  where tgr :: Gr Char ()
        tgr = read . show $ a

