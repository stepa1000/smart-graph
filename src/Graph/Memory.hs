-- | Memory Graph. Based on fgl and aeson library with sequence
--
-- To implement a decision-making algorithm based on the history
-- of expiriance gained.
--
-- First,there is a stage of empirical observation without attempting
-- any actions. Their received data completely or by any choice from
-- a memory graph, where the vertice are part of the state of the data,
-- the edges are the previos history or an approximation of the patterns
-- of data change. After the specifed time, the algorithm starts procesing
-- the acquired knowledge graph.
--
-- Finds the articulation points and BCC:
-- The allow you to discover relevant data from those who have lost
-- a causal relationship. This may happen due to the fact that the input
-- data will beginyo lie an another connected component or the edges
-- can lead through this point, thereby changing the connected component.
-- It can olso be used to find understudied data and specificaly pass through
-- points to have a chance to gain specific experience.
--
-- BFS:
-- Formating of path from different conectivity components. This can be useful
-- for transitioning from more predictable components to less predicatable ones
-- and vice versa, or to test the controllability of a component.
-- Although DFS can olso be used for this in fgl, dfs is more functional,
-- but bfs and dfs can be used in parallel to achieve computational speed.
--
-- DFS:
-- Can be used to bypass the specefied nodes, and for filtering, switching to
-- preferred ones. For exemple, to bypass articulation points, the control
-- of reaching which will be acceptable. Or divide the graph elements into
-- controlled and uncontrolled. Topological sorting is able to set a parameter
-- for the behavior, you can be on the extreme nodes or in the inside.
-- Fiding strong connectedness components allows you to analize groups
-- of graph elements, and from a behavior and hashing based on the given.
--
-- Dominators:
-- Finding available nodes. You can classify a vertex by the number
-- of domains.
--
-- GVD:
-- Finds sets paths from vertices or to vertices. Can be used to find any path
-- to or from a point.
--
-- Indip:
-- Another way to parameterize behavior is by the preference of given points
-- for study.
--
-- Thanks to the above, it is possible to generate paths and filter according
-- to the desired criteria. For example, move throught special points or avoid
-- the absence of control, or vice versa, run uncontrolled scenarios for as long
-- as posible. It is posible to manually design instincts to prioritize components
-- of interesting by examing activations from inputs. A truly interesting
-- result can be obtained if it is posible to train the graph on the same experience,
-- that is, the history and processing are build on the current possible behavior options.
module Graph.Memory where

import Data.Aeson
import Data.Graph.Inductive
import Data.Sequence

-- | Memory Graph. The vertices and edges are
-- sterelized into a json object, but the data in the edges
-- is contained  in sequnces.
--
-- The fgl library implements many algorithm  on graph,
-- and data sterilization allows you to add functionality
-- to the existing one withot constantly changing the types
-- of edges and vertices.
--
-- This graph can be hashed and deleted old data with
-- the ability to recover forgotten memory by enumiration
-- which will allow strong and analyzing large graphs.
--
-- It is important that the processing occurs throught
-- the logic monad. since the structure can contain both
-- records and a constructors or multiplications and
-- additions. The support of this feature will allow
-- creating complex conditions for the transitions
-- between in the vertices.
--
-- The used implementation of the graph has difficuls with
-- taking elements, because of which  you have to have auxiliary
-- structures for traversing the graph or accessing the elements.
-- But by the fact that their boxes are avalilable to many
-- algorithms over a given implementation of the graph.
type MemoryGraph gr = gr (Value) (Seq Value)
