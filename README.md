# ChessGame
Simple chess game made for learning basics of Scala. WIP

## Board and figures
For now board looks like this:

 ```
    0   1   2   3   4   5   6   7
   +---+---+---+---+---+---+---+---+
0 | R | N | B | K | Q | B | N | R |
   +---+---+---+---+---+---+---+---+
1 | P | P | P | P | P | P | P | P |
   +---+---+---+---+---+---+---+---+
2 |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+
3 |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+
4 |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+
5 |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+
6 | p | p | p | p | p | p | p | p |
   +---+---+---+---+---+---+---+---+
7 | r | n | b | k | q | b | n | r |
   +---+---+---+---+---+---+---+---+
  ```
  
Uppercase letters represents black pieces, lowercase white ones.

* P/p: pawn
* R/r: rook
* N/n: knight
* B/b: bishop
* K/k: king
* Q/q: queen

## Game process
1. Choose position(row, column) of piece you want to move.
2. Select index of available position you want your piece to move to.

If you want to choose another piece enter `-1`

White makes move first
```
    0   1   2   3   4   5   6   7
  +---+---+---+---+---+---+---+---+
0 | R | N | B | K | Q | B | N | R |
  +---+---+---+---+---+---+---+---+
1 | P | P | P | P | P | P | P | P |
  +---+---+---+---+---+---+---+---+
2 |   |   |   |   |   |   |   |   |
  +---+---+---+---+---+---+---+---+
3 |   |   |   |   |   |   |   |   |
  +---+---+---+---+---+---+---+---+
4 |   |   |   |   |   |   |   |   |
  +---+---+---+---+---+---+---+---+
5 |   |   |   |   |   |   |   |   |
  +---+---+---+---+---+---+---+---+
6 | p | p | p | p | p | p | p | p |
  +---+---+---+---+---+---+---+---+
7 | r | n | b | k | q | b | n | r |
  +---+---+---+---+---+---+---+---+
Please enter position(row, column) of a figure to move: 
7 1
Available moves:
0 (5,0)
1 (5,2)
Select move: 
0
```
