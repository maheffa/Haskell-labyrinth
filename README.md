# Haskell-labyrinth
Implementation of a labyrinth searching algorithm using a purely function language (Haskell) with visual animation (Gloss library)

The first day I walked into functionnal programming class and reading the first lines of functionnal programming book, I was fond of it. This project is for proving you can do amazing things even with a pure fonctionnal programming language such as Haskell. My professor loved it.

The project is about solving maze problems. 
  - Input files: Text file containing structure of the maze, where 'x' - wall, '.' - free path, 'R' - initial position of the robot, 'O' - point that robot need to get to (can be several point).
  - Output: An animation of the robot moving through the maze to get to all the 'O' points.

I experimented with both DFS (recursiveSearch :: Maze -> [Move]) and BFS (breadthFirstSearch :: Maze -> [Move]) search algorithm. Changing 'recursiveSearch' to 'breadthFirstSearch' in 'runAlgo' will change from one search to another.

Code description:
  - Main.hs: Entry point of the program. It reads maze structure from the input file using 'getMazeFromFile' function. Then run the search using 'runAlgo'. The received output path will be used with Gloss library to output an animation.
  - Algo.hs: contains the DFS and BFS algorithm
  - Maze.hs: contains utility functions for storing datastructure, moving robot, reading from file, sending succession of picture to Gloss in order to launch the animation.

Running the compiled program is as simple as: './main <maze_text_file>'
