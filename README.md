# assignment01

Run the program with:  <br />
stack run

Or with executable:  <br />
stack build <br />
stack exec assignment01-exe


The program has three modes:  <br />
player vs player (stack run 1)  <br />
player vs computer (stack run 2)  <br />
computer vs computer (stack run 3)

You can also list these modes with -h or --help at the end of the run command (stack run -h)

The object of the game is to get 3 in a row(in row, column or diagonal). You choose which spot you want to mark by choosing a position from 1-9. the board has these positions:

| | | |
| :---: | :---: | :---: |
|  1 |  2 |  3 |
|  4 |  5 |  6 |
|  7 |  8 |  9 |

You can also turn the board by adding 'left' or 'right' at the end of your commnad. (example: 3 left). It will then mark the position, then swap position 1 and 3, and then rotate the board. If the player does an illigal command or mark a spot that is occupied, the player loses. The computer will never do an illigal command.

