module Smart.Memory.Base where

-- import Control.Concurrent.STM.TVar
import Data.Bitraversable
import Data.Foldable as Fo
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.HashMap.Lazy as HM
import Data.HashSet as HS
import Data.Hashable
import Data.Maybe
import Data.Proxy
import Data.Sequence as S
import Data.Tuple (swap)
import Data.Vector as V
import Smart.Element.Base
import Smart.Proc.Run

type Memory gr a b = gr a (Seq b)

type EdgeMem a p = (Either (Elem a) (p a), Either (Elem a) (p a), Seq (Either (Elem a) (p a)))

-- chunksOf length

totalChunks :: Seq a -> [Seq a]
totalChunks s = f l s
  where
    f i _ | i == 0 = []
    f i s = (S.toList $ chanksOf (i - 1) s) : (f (i - 1) s)
    l = S.length s

genProcFromSeq :: GenProc p a => Proxy p -> Seq (Vector a, Vector a) -> Seq (Vector (EdgeMem a p))
genProcFromSeq pr s =
  fmap (fmap (\(i, (x, y)) -> (Left (i, x), Left (i, y), S.singleton $ Right $ genP x y)) . V.indexed . (\(x, y) -> V.zip x y)) s

totalMemory ::
  (GenProc p a, DynGraph gr) =>
  Proxy p ->
  Seq (Vector a, Vector a) ->
  Memory gr (Either (Elem a) (p a)) (Either (Elem a) (p a))
totalMemory pr s = undefined
  where
    szh = S.zip sz s
    sz = genProcFromSeq pr s
