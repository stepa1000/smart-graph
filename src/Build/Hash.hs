module Build.Hash where

import Data.Graph.Inductive.Graph
import Data.HashMap.Lazy
import Data.Hashable

hashMapNode :: (Graph gr, Hashable a) => gr a b -> HashMap a Node
hashMapNode = fold . fmap (\(n, a) -> singleton a n) . labNodes

hashMapEdge :: (Graph gr, Hasheble b) => gr a b -> HashMap b (Node, Node)
hashMapEdge = fold . fmap (\(n1, n2, b) -> singleton b (n1, n2)) . labEdges

graphToList :: (Graph gr, Hasheble a, Hasheble b) => gr a b -> ([a],[(a,a,b)])
graphToList gr = 
  where
    hmn = hashMapNode gr
    hme = hashMapEdge gr
