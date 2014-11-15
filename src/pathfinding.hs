
{-# LANGUAGE MultiParamTypeClasses #-}


module Pathfinding ( breadthFirstSearch, depthFirstSearch, bestFirstSearch, uniformCostSearch, aStarSearch ) where


import Data.Maybe          as Maybe
import Data.List           as List
import Data.Sequence       as Sequence
import Data.Heap           as Heap
import Data.HashMap.Strict as Map
import Data.Hashable       as Hashable

import Graph



findPath :: (Graph graphT stateT) => graphT -> stateT -> Map.HashMap stateT stateT -> [stateT]
findPath g state m
  | Graph.start g == state = []
  | otherwise = state : findPath g (fromJust parent) m
  where
    parent = Map.lookup state m

search :: (Graph graphT stateT) => graphT -> Seq stateT -> Map.HashMap stateT stateT -> [stateT] -> (Seq stateT -> stateT) -> (Seq stateT -> Seq stateT) -> ([stateT], [stateT])
search g q considered path opFirst opRest
  | Sequence.null q = error "No path from the start to the goal could be found"
  | Graph.goal g == state = (List.reverse path, findPath g (Graph.goal g) considered)
  | otherwise = search g q' considered' (state : path) opFirst opRest
  where
    state       = opFirst q
    neighbors   = List.filter (\ s -> isNothing (Map.lookup s considered)) (Graph.getNeighbors g state)
    q'          = opRest q >< Sequence.fromList neighbors
    considered' = Map.union considered (Map.fromList (List.zip neighbors (List.repeat state))) -- this union is O(n+m) consider looking up the states in path instead or doing multiple inserts

breadthFirstSearch :: (Graph graphT stateT) => graphT -> ([stateT], [stateT])
breadthFirstSearch g =
  search g (Sequence.singleton start) (Map.singleton start start) []
    (\ q -> Sequence.index q 0)
    (Sequence.drop 1)
  where
    start = Graph.start g

depthFirstSearch :: (Graph graphT stateT) => graphT -> ([stateT], [stateT])
depthFirstSearch g =
  search g (Sequence.singleton start) (Map.singleton start start) []
    (\ q -> index (Sequence.drop (Sequence.length q - 1) q) 0)
    (\ q -> Sequence.take (Sequence.length q - 1) q)
  where
    start = Graph.start g

findPriorityPath :: (Graph graphT stateT) => graphT -> stateT -> Map.HashMap stateT (stateT, Int) -> [stateT]
findPriorityPath g state m
  | Graph.start g == state = []
  | otherwise = state : findPriorityPath g (fst (fromJust (Map.lookup state m))) m

prioritySearch_r :: (Graph graphT stateT) => graphT -> (stateT -> stateT -> Int) -> (graphT -> stateT -> Int) -> MinPrioHeap Int (stateT, stateT, Int) -> Map.HashMap stateT (stateT, Int) -> [stateT] -> ([stateT], [stateT])
prioritySearch_r g edgeFn heuristicFn q dead path
  | Heap.null q           = error "No path from the start to the goal could be found"
  | Graph.goal g == state = (List.reverse path, findPriorityPath g (Graph.goal g) dead')
  | otherwise             = prioritySearch_r g edgeFn heuristicFn q' dead' (state : path)
  where
    ((_, (state, parent, edgeCost)), restQ) = fromJust (Heap.view q)
    (visited, neighbors) = List.partition (\ s -> member s dead) (Graph.getNeighbors g state)
    totalCost     s      = edgeCost + edgeFn state s + heuristicFn g s
    totalEdgeCost s      = edgeCost + edgeFn state s
    toConsider           = List.filter (\ s -> totalEdgeCost s < snd (fromJust (Map.lookup s dead))) visited
    considerations       = fmap (\ s -> (s, (state, totalEdgeCost s))) toConsider
    (toAdjust, keep)     = Heap.partition (\ (_, (s, _, _)) -> List.elem s neighbors) restQ
    adjustments          = fmap (\ (c, (s, p, ec)) -> if totalEdgeCost s < ec then (totalCost s, (s, state, totalEdgeCost s)) else (c, (s, p, ec))) (Heap.toList toAdjust)
    newNeighbors         = neighbors \\ fmap (\ (_, (s, _, _)) -> s) adjustments
    q'                   = Heap.union keep (Heap.union (Heap.fromList (fmap (\ s -> (totalCost s, (s, state, totalEdgeCost s))) newNeighbors)) (Heap.fromList adjustments))
    dead'                = Map.union (Map.fromList considerations) (Map.union (Map.singleton state (parent, edgeCost)) dead)

prioritySearch :: (Graph graphT stateT) => graphT -> (stateT -> stateT -> Int) -> (graphT -> stateT -> Int) -> ([stateT], [stateT])
prioritySearch g edgeFn heuristicFn =
  prioritySearch_r g edgeFn heuristicFn (Heap.singleton (0, (start, start, 0))) Map.empty []
  where
    start = Graph.start g

bestFirstSearch :: (Graph graphT stateT) => graphT -> (graphT -> stateT -> Int) -> ([stateT], [stateT])
bestFirstSearch g heuristicFn = prioritySearch g (\ _ _ -> 0) heuristicFn

uniformCostSearch :: (Graph graphT stateT) => graphT -> (stateT -> stateT -> Int) -> ([stateT], [stateT])
uniformCostSearch g edgeFn = prioritySearch g edgeFn (\ _ _ -> 0)

aStarSearch :: (Graph graphT stateT) => graphT -> (stateT -> stateT -> Int) -> (graphT -> stateT -> Int) -> ([stateT], [stateT])
aStarSearch g edgeFn heuristicFn = prioritySearch g edgeFn heuristicFn



