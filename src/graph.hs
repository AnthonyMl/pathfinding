

{-# LANGUAGE MultiParamTypeClasses,FunctionalDependencies #-}


module Graph where


import Data.Hashable as Hashable


class (Eq state, Hashable state) => Graph graph state | graph -> state where
  getNeighbors :: graph -> state -> [state]
  start        :: graph -> state
  goal         :: graph -> state




