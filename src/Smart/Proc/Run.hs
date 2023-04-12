{-# LANGUAGE MultiParamTypeClasses #-}

module Smart.Proc.Run where

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

class Proc p a where
  runP :: p a -> a -> Maybe a

class Proc p a => GenProc p a where
  genP :: a -> a -> p a
