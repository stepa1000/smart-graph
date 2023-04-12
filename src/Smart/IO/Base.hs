module Smart.IO.Base where

-- import Control.Concurrent.STM.TVar
import Data.Bitraversable
import Data.Foldable as Fo
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.NodeMap
import Data.HashMap.Lazy as HM
import Data.HashSet as HS
import Data.Hashable
import Data.Maybe
import Data.Sequence as S
import Data.Tuple (swap)
import Data.Vector as V

class IOElement a where
  inputE :: IO (Vector a)
  outputE :: Vector a -> IO ()

interElement :: IOElement a => Int -> (Seq (Vector a, Vector a) -> Vector a -> IO (Vector a)) -> IO ()
interElement s f = do
  -- tvseq <- newTVarIO S.empty
  va <- inputE
  g S.empty va
  where
    g s va | V.null va = return ()
    g s va = do
      vau <- f s va
      outputE vau
      vai <- inputE
      g (s <> (S.singleton (va, vau))) vai
