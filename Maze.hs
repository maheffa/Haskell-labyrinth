module Maze where

import Debug.Trace
import Data.List
import System.IO
import Graphics.Gloss
import Graphics.Gloss.Data.Picture

type Maze = [String]
data Move = UP | DOWN | LEFT | RIGHT deriving (Show, Eq)
allMoves = [UP, DOWN, LEFT, RIGHT]

{-
Maze file contain maze map, where character . is for empty
x if for wall, R for the robot, and O for the objective
-}

caseSize = 50::Int
caseSizeF = 50::Float

applyMovesOnMaze :: Maze -> [Move] -> IO()
drawMaze :: Maze -> IO ()
getCoordinate :: Picture
getMazePicture :: Maze -> Int -> Int -> Picture
getMazeFromFile :: String -> IO Maze
getMazeSuccession :: Maze -> [Move] -> [Maze]
getNextPosition :: (Int, Int) -> Move -> (Int,Int)
getPosition :: Maze -> Char -> (Int, Int)
getPositions :: Maze -> Char -> [(Int,Int)]
getPossibleMove :: Maze -> [Move]
getRobotPosition :: Maze -> (Int, Int)
moveRobot :: Maze -> Move -> Maze
readMaze :: String -> IO Maze
setPosition :: Maze -> Char -> (Int, Int) -> Maze


readMaze fileName =
	do
		fileHandle <- openFile fileName ReadMode
		contents <- hGetContents fileHandle
		return (lines contents)
		

getMazePicture m i j = 
	let drawRow row y = foldr1 (++) (map (\(c,x) -> --trace ("drawing image of "++show(c)++" at ("++show(x)++","++show(y)++")") $
		drawImage c (x,y)) (zip row [-fromIntegral(j)/2..]))
		where
			drawImage t (x,y) 
				| t == '.' = [drawSquare white (x,y)]
				| t == 'x' = [drawSquare black (x,y)]
				| t == 'R' = drawImage '.' (x,y) ++ [drawObject green (x,y) 0.5]
				| t == 'O' = drawImage '.' (x,y) ++ [drawObject red (x,y) 0.2]
				| otherwise = [drawSquare white (x,y)]
				where
					drawSquare c (u,v) = -- trace ("drawing "++show(c)++" square at ("++show(u)++","++show(v)++")") $
						Translate (caseSizeF*(u+1)) (caseSizeF*(v-1)) $ Color c (Polygon [(0,0),(0,caseSizeF),(-caseSizeF, caseSizeF),(-caseSizeF, 0)])
					drawObject c (u,v) sc = Translate (caseSizeF*(u+tmv)) (caseSizeF*(v-tmv)) $ Scale sc sc $ drawSquare c (0,0) where tmv = (1-sc)/2
	in
		Pictures $ foldr1 (++) $ map (\(r,n) -> drawRow r n) (zip m (iterate (-1+) (fromIntegral(i)/2)))


getCoordinate =
	let m = 1000
	in Pictures $ [Line [(0,-m),(0,m)], Line[(-m,0),(m,0)]] ++ (positionOverY m) ++ (positionOverX m)
	where
		positionOverX m = map (\t -> Translate (100*t) 0 (Scale 0.1 0.1 (Text (show (100*t))))) [-m/100 .. m/100]
		positionOverY m = map (\t -> Translate 0 (100*t) (Scale 0.1 0.1 (Text (show (100*t))))) [-m/100 .. m/100]


drawMaze m = display (InWindow "My Window" (j*caseSize, i*caseSize) (10, 10)) (greyN 0.5) $
	trace ("drawing maze with size "++show(i)++"x"++show(j)++"") $
	--Pictures [getMazePicture m i j, getCoordinate]
	getMazePicture m i j
	where
		j = length $ head m
		i = length m


getMazeFromFile f = ((openFile f ReadMode) >>= \fh -> hGetContents fh) >>= \contents -> return (lines contents)

frameSec = 0.5::Float
applyMovesOnMaze mz mvs = --trace ("Applying moves : "++show(mvs))
	animate (InWindow "Robot control over labyrinth - Coursework of MANITRARIVO Adama Mahefa" (j*caseSize, i*caseSize) (10, 10)) (greyN 0.5) $
	(\time -> getMazePicture ((getMazeSuccession mz mvs) !! (frameNum time)) i j )
	where	
		k time = if floor(time/frameSec) < 0 then 0 else floor(time/frameSec)
		j = length $ head mz
		i = length mz
		frameNum time = min (length mvs) (k time)
		

getMazeSuccession mz mvs = 
	let ccat (mzlist, maze) move = (mzlist ++ [nextMaze], nextMaze)
		where nextMaze = moveRobot maze move
	in
		fst $ foldl ccat ([mz], mz) mvs
		

moveRobot mz mv = --trace ("moving robot from position "++show(curX, curY)++" "++show(mv)) $
	setPosition (if mz !! nextX !! nextY == 'O' then clearMaze mz else setPosition mz 'v' (curX, curY)) 'R' (nextX, nextY)
	where 
		(nextX, nextY) = getNextPosition (curX, curY) mv
		(curX, curY) = getRobotPosition mz
		clearMaze m = map (\s -> map (\c -> if c=='v' || c=='R' then '.' else c) s) m


getNextPosition (x, y) m
	| m == UP = (x-1, y)
	| m == DOWN = (x+1, y)
	| m == LEFT = (x, y-1)
	| m == RIGHT = (x, y+1)

getPositions mz c = foldr (++) []	$ map (\l ->  zip [l,l..] (getContainingColumns mz l)) $ getContainingLines mz
	where
		getContainingLines m = filter (>=0) (map (\(a,b) -> if c `elem` a then b else -1) (zip m [0..]))
		getContainingColumns m l = filter (>=0) (map  (\(a,b) -> if a==c then b else -1) (zip (m !! l) [0..]))

getPosition mz c = head $ getPositions mz c

getRobotPosition mz = getPosition mz 'R'
	
setPosition mz w (x,y) = map (\(a,b) -> if b==x then map (\(c,d) -> if d==y then w else c) (zip a [0..]) else a) (zip mz [0..])

getPossibleMove mz = 
	filter (\mv -> notWall mz (getNextPosition (x,y) mv)) allMoves
	where
		notWall mz (x,y) = (mz !! x !! y == '.') || (mz !! x !! y == 'O')
		(x,y) = getRobotPosition mz