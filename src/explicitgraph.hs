
{-# LANGUAGE MultiParamTypeClasses #-}

-- also directed
module ExplicitGraph where


import Data.Maybe          as Maybe
import Data.List           as List
import Data.HashMap.Strict as Map

import Graph



data ExplicitGraph = ExplicitGraph {
  size  :: Int,
  start :: Int,
  goal  :: Int,
  edges :: Map.HashMap Int [(Int, Int)]
  }

create :: Int -> Int -> Int -> [(Int, [(Int, Int)])] -> ExplicitGraph
create size start goal edges = ExplicitGraph size start goal (Map.fromList edges)


edgeCost :: ExplicitGraph -> Int -> Int -> Int
edgeCost g a b = fromJust (List.lookup b (fromJust (Map.lookup a (ExplicitGraph.edges g))))


instance Graph ExplicitGraph Int where
  getNeighbors g state
    | isNothing result = []
    | otherwise = fmap fst (fromJust result)
    where
      result = Map.lookup state (ExplicitGraph.edges g)
  start                = ExplicitGraph.start
  goal                 = ExplicitGraph.goal



