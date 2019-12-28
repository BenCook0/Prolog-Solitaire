Ben Cook 30037563

The peg solitaire program can be run using the following commands:

peg(crossbow)
peg(longbow)
peg(mostlydead)
peg(halfdead)
peg(notquitedead)
peg(full)

Note that some code (such as some of the pagoda functions) is not actually used, but was kept in for the sake of completeness. After thorough testing, I found that the best pagoda functions to use were the first (simple), second (center) and the second but rotated 90 degrees (centerF).

Also note that the program prints the starting board, then calculates and prints the moves, and then prints the goal board. Since at the end, the final step and the goal board are equal, then we know that it is solved correctly. Furthermore, the professor said that it is unnecessary to prompt the user to press a key to step through the solution, so the boards print out all at once.