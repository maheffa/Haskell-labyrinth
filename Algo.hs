module Algo where

import Debug.Trace
import Maze

runAlgo :: Maze -> [Move]
recursiveSearch :: Maze -> [Move]
breadthFirstSearch :: Maze -> [Move]

isFinal :: Maze -> Bool
getMinimalListLength :: [[a]] -> [a]

runAlgo m = trace ("Object(s) position at: "++show(getPositions m 'O')++"\nSolution : "++show(recursiveSearch m)) $
				recursiveSearch m

recursiveSearch mz = getBestWay mz []
	where
		getBestWay cmz path
			| isFinal cmz = path
			| length (getPossibleMove cmz) == 0 = []
			| otherwise = let getBestWayByMove mv = getBestWay ncmz (path++[mv]) where ncmz = moveRobot cmz mv
							in --trace ("getting path through "++show(path)++"\nfrom "++show(getRobotPosition cmz)++"\nWhere possible moves are: "++show(getPossibleMove cmz)++"\n"++unlines cmz) $ 
								getMinimalListLength $ map getBestWayByMove $ getPossibleMove cmz
									

breadthFirstSearch mz = findIn [(mz,[]::[Move])]
	where
		findIn mazePathList
			| length mazePathList == 0 = []
			| length (succeed mazePathList) > 0 = {-trace ("DONE WITH"++show(snd $ head $ succeed mazePathList))-} snd $ head $ succeed mazePathList
			| otherwise = findIn $ foldr (++) [] $ map (\(m,p) -> map (\mv -> (moveRobot m mv, p++[mv])) (getPossibleMove m)) mazePathList
				where
					succeed mazePathList = --trace ("checking in "++(printLayer mazePathList)) $
						filter (\(maze, path) -> isFinal maze) mazePathList
					printLayer mazePathList = foldr (++) "" $ map (\(mz, p) -> show ("from "++show(p)++"\n"++(unlines mz)++"\n\n")) mazePathList

------------------------------------------------------------------------------------------------------

getNextConfiguration (mz, (x, y)) mv = (moveRobot mz mv, getNextPosition (x,y) mv)

getMinimalListLength t =
	let 
		sortByLength [] = []
		sortByLength (a:l) = (sortByLength $ filter (\ll -> length ll < length a) l) ++ [a] ++ (sortByLength $ filter (\ll -> length ll > length a) l) 
		filteredList l = filter (\ll -> 0 /= length ll)  l
	in	
		if length (filteredList t) == 0 then []  else head $ sortByLength (filteredList t)

isFinal mz = length (getPositions mz 'O') == 0