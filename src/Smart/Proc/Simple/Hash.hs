{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Smart.Proc.Simple.Hash where

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
import Smart.Element.Base
import Smart.Proc.Run

newtype ProcHash a = ProcHash (Int, a)

procHash :: Hashable a => a -> a -> ProcHash a
procHash ai au = ProcHash (hash ai, au)

runProcHash :: Hashable a => ProcHash a -> a -> Maybe a
runProcHash (ProcHash (h, u)) a =
  if (hash a) == h
    then Just u
    else Nothing

instance Hashable a => Proc ProcHash a where
  runP = runProcHash

instance Hashable a => GenProc ProcHash a where
  genP = procHash
