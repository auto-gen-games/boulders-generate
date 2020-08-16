# Generator for a platform puzzle game

This Scala project generates puzzles for a simple platform puzzle in which a player traverses a maze, moving rocks around to reach a diamond and then an exit.

The challenge for generating puzzles is that there is an enormous space of possibilities with a relatively small grid, and most are either impossible or trivially easy.

The generator works by iteratively searching for the most likely ways to make impossible mazes possible, and then the most likely obstacles to make possible mazes harder.

More details to come.
