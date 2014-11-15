

--{-# LANGUAGE NoImplicitPrelude #-}

module Main where


import Graphics.UI.GLUT
import Control.Monad
import Data.List as List
import Data.IORef as IORef
import Data.HashSet as Set

import Vec2
import Pathfinding
import Grid2D
import ExplicitGraph
import Graph



color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)


gridA = Grid2D.create 41 (Vec2 20 20) (Vec2 39 39)
gridB = Grid2D.create 41 (Vec2 20 20) (Vec2 7 7)
gridC = Grid2D.addObstacles (Grid2D.create 41 (Vec2 20 20) (Vec2 39 31)) (Vec2.fromList
  (List.zip [10..40] (List.repeat 30) ++ List.zip [0..17] (List.repeat 26) ++ [(10, 29), (10, 28), (11, 28), (17, 25), (17, 24), (16, 24)]))
gridD = Grid2D.addObstacles (Grid2D.create 41 (Vec2 1 1) (Vec2 39 39)) (Vec2.fromList
  (List.zip [6..34] [10..38] ++ List.zip [35..40] (List.repeat 38)))
gridE = Grid2D.addObstacles (Grid2D.create 41 (Vec2 30 10) (Vec2 24 27)) (Vec2.fromList
  (List.zip [8..36] [10..38] ++ [(37, 38), (38, 38), (39, 38), (40, 38)]))
gridF = Grid2D.addObstacles (Grid2D.create 41 (Vec2 10 10) (Vec2 35 35)) (Vec2.fromList
  (List.zip [15..25] (List.repeat 25) ++ List.zip (List.repeat 25) [20..25]))


explicitGraphA = ExplicitGraph.create 12 0 5
  [(0, [(1,1),(6,5)]), (1, [(2,1)]), (2, [(3,1)]), (3, [(4,1),(8,1)]), (4, [(5,1)]), (6, [(5,5), (7,1)]), (8, [(9,1)]), (10, [(11,1)]), (11, [(10,1)])]

drawSquare (px, py) size = do
  vertex3f px py 0
  vertex3f px (py + size) 0
  vertex3f (px + size) (py + size) 0
  vertex3f (px + size) py 0

gridDrawScale grid = 2 / (fromIntegral (Grid2D.size grid) :: GLfloat)

transformVectorForDrawing grid (Vec2 x y) =
  (-1 + fromIntegral x * scale, -1 + fromIntegral y * scale)
  where
    scale = gridDrawScale grid

drawGrid :: Grid2D -> ([Vec2], [Vec2]) -> Int -> IO ()
drawGrid g (visited, path) count =
  renderPrimitive Quads $ do
    color3f 0.125 0.25 0.5
    drawSquare (-1, -1) 2
    color3f 0.2 0.2 0.2
    mapM_ (\ v -> drawSquare (transformVectorForDrawing g v) scale) (Set.toList (Grid2D.obstacles g))
    color3f 0.8 0.2 0.2
    mapM_ (\ v -> drawSquare (transformVectorForDrawing g v) scale) (List.take count visited)
    color3f 0.8 0.2 0.8
    mapM_ (\ v -> drawSquare (transformVectorForDrawing g v) scale) (List.take (count - List.length visited) path)
    when (count < List.length visited) (do
      color3f 0.8 0.8 0.8
      drawSquare (transformVectorForDrawing g ((!!) visited count)) scale)
    color3f 0.8 0.8 0.2
    drawSquare (transformVectorForDrawing g (Graph.start g)) scale
    color3f 0.2 0.8 0.2
    drawSquare (transformVectorForDrawing g (Graph.goal g)) scale
  where
    scale = gridDrawScale g

printExplicitGraph g costFn algo = do
  putStrLn ("length: " ++ show (List.length path))
  putStrLn ("cost: " ++ show (List.foldr (\ (a, b) cost -> cost + costFn a b) 0 (List.zip (ExplicitGraph.start g : rpath) rpath)))
  putStrLn (show (ExplicitGraph.start g : rpath))
  putStrLn ""
  where
    (_, path) = algo g
    rpath = List.reverse path

printExplicitGraphs = do
  putStrLn "breadthFirstSearch eGA: "
  printExplicitGraph explicitGraphA (ExplicitGraph.edgeCost explicitGraphA) breadthFirstSearch
  putStrLn "uniformCostSearch eGA: "
  printExplicitGraph explicitGraphA (ExplicitGraph.edgeCost explicitGraphA) (\ g -> uniformCostSearch g (ExplicitGraph.edgeCost g))

main :: IO ()
main = do
  (programName, arguments) <- getArgsAndInitialize
  initialDisplayMode       $= [DoubleBuffered]
  initialWindowPosition    $= Position 100 50
  initialWindowSize        $= Size 640 640
  window                   <- createWindow "Simple Pathfinding"
  count                    <- newIORef 0
  time                     <- get elapsedTime
  lastTime                 <- newIORef time
  displayCallback          $= display count
  idleCallback             $= Just (idle lastTime count)
  printExplicitGraphs
  mainLoop

display :: IORef Int -> DisplayCallback
display count = do
  clear [ ColorBuffer ]
  c <- get count
--  drawGrid gridE (breadthFirstSearch gridE) c
--  drawGrid gridE (depthFirstSearch gridE) c
--  drawGrid gridC (bestFirstSearch gridC (\ g s -> let gs = Graph.goal g - s in Vec2.dot gs gs)) c
--  drawGrid gridE (bestFirstSearch gridE (\ g s -> let gs = Graph.goal g - s in Vec2.dot gs gs)) c
--  drawGrid gridE (uniformCostSearch gridE (\ _ _ -> 1)) c
  drawGrid gridC (aStarSearch gridC (\ _ _ -> 1) (\ g s -> let gs = Graph.goal g - s in floor $ sqrt $ fromIntegral (Vec2.dot gs gs))) c
--  drawGrid gridD (aStarSearch gridD (\ _ _ -> 1) (\ g s -> let gs = Graph.goal g - s in abs (Vec2.x gs) + abs (Vec2.y gs))) c
  swapBuffers

idle :: IORef Int -> IORef Int -> IdleCallback
idle lastTime count = do
  time <- get elapsedTime
  last <- get lastTime
  when (time - last > updateInterval) (do
    modifyIORef' count    (+ 1)
    modifyIORef' lastTime (+ updateInterval))
  postRedisplay Nothing
  where
    updateInterval = 30 -- in ms
























