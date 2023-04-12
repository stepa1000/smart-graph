module Build.Hash where

import Data.Bitraversable
import Data.Foldable as Fo
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.HashMap.Lazy as HM
import Data.HashSet as HS
import Data.Hashable
import Data.Maybe
-- import Data.Sequence as S
import Data.Tuple (swap)

hashMapNode :: (Graph gr, Hashable a) => gr a b -> HashMap a Node
hashMapNode = fold . fmap (\(n, a) -> HM.singleton a n) . labNodes

swapHM :: (Hashable a, Hashable b) => HashMap a b -> HashMap b a
swapHM = HM.fromList . fmap swap . HM.toList

hashMapEdge :: (Graph gr, Hashable b) => gr a b -> HashMap b [(Node, Node)]
hashMapEdge = Fo.foldr (unionWith (<>)) HM.empty . fmap (\(n1, n2, b) -> HM.singleton b [(n1, n2)]) . labEdges

graphToList :: (Graph gr, Hashable a, Hashable b) => gr a b -> ([a], [(a, a, b)])
graphToList gr = (fmap fst $ HM.toList hmn, foldMapWithKey (\b lnn -> fmap (\(x, y) -> (shmn HM.! x, shmn HM.! y, b)) lnn) hme)
  where
    hmn = hashMapNode gr
    shmn = swapHM $ hashMapNode gr
    hme = hashMapEdge gr

unionGraph :: (Graph gr, Hashable a, Hashable b) => gr a b -> gr a b -> ([a], [(a, a, b)])
unionGraph gr1 gr2 =
  (fmap fst $ HM.toList $ hmn1 <> hmn2, HS.toList $ (f hme1 shmn1) <> (f hme2 shmn2))
  where
    f hme hmn = foldMapWithKey (\b lnn -> fold $ fmap (\(x, y) -> HS.singleton (hmn HM.! x, hmn HM.! y, b)) lnn) hme
    hmn1 = hashMapNode gr1
    shmn1 = swapHM $ hashMapNode gr1
    hme1 = hashMapEdge gr1
    hmn2 = hashMapNode gr2
    shmn2 = swapHM $ hashMapNode gr2
    hme2 = hashMapEdge gr2

unionGraphNMM :: (Graph gr, Hashable a, Hashable b, Ord a, DynGraph gr) => gr a b -> gr a b -> NodeMapM a b gr ()
unionGraphNMM gr1 gr2 = (\(x, y) -> insMapNodesM x >> insMapEdgesM y) $ unionGraph gr1 gr2

unionGraphNMME ::
  (Graph gr, Hashable a1, Hashable b1, Ord a1, Hashable a2, Hashable b2, Ord a2, DynGraph gr) =>
  gr a1 b1 ->
  gr a2 b2 ->
  NodeMapM (Either a1 a2) (Either b1 b2) gr ()
unionGraphNMME gr1 gr2 = unionGraphNMM (nemap Left Left gr1) (nemap Right Right gr2)

bisequenceAGrE :: (DynGraph gr, Applicative f) => gr a (Either (f b1) (f b2)) -> gr a (f (Either b1 b2))
bisequenceAGrE = emap bisequenceA
