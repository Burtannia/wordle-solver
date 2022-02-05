# Wordle Solver

A basic solver for Wordle. Reads input as follows:
- 0 = Grey
- 1 = Yellow
- 2 = Green

So if the squares were Grey, Yellow, Yellow, Green, Grey then the input would be 01120.

Works by narrowing down a dictionary via a set of filters then choosing the word
from the remaining dictionary that has the most common character in each space.

I have also experimented with choosing the word that has the least common character
in each space which seems to outperform the former in some cases. Mainly that it
avoids the case where there are several words remaining that differ only in 1 or 2
characters.

The optimal solution would likely require allowing the bot to "backtrack" and choose
a word that cannot possibly be the answer but that contains the largest number
of characters that need to be ruled out. This will be the next update if I get round to it.