import Debug.Trace
import Graphics.Gloss
import Maze
import Algo (runAlgo)
import System.Environment (getArgs)
import Control.Monad
import System.CPUTime
import Text.Printf

main =
	do
		start <- getCPUTime
		args <- getArgs
		if length args == 0 then putStrLn("command: Main.exe <labyrinth file (grill3.txt)>")
		else 
			do 
				putStrLn("Reading maze from file xs"++show(args))
				maze <- (getMazeFromFile (head args))
				moves <- (return (runAlgo maze))
				putStrLn(show(moves)++"\nRunning graphics ...")
				end <- getCPUTime
				let diff = (fromIntegral (end - start)) / (10^12)
				printf "Computation time: %0.3f sec\n" (diff :: Float)
				applyMovesOnMaze maze moves