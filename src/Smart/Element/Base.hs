module Smart.Element.Base where

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

type Elem a = (Int, a)

getElem :: Int -> Vector a -> Elem a
getElem i v = (i, v V.! i)

setElem :: Elem a -> Vector a -> Vector a
setElem (i, a) v = v V.// [(i, a)]
