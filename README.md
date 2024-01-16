# SudokuSolver


This OCaml project aims to solve Sudokus with mostly logic and occasional guesswork, with the following steps:
1. Read in the specification from a text file
2. For each cell in the Sudoku create a list of ints representing the possible values in the cell
3. Use the rules of Sudoku and logic to trim down all the lists until each only contains a single value
4. If no more progress can be made with logic then guess a value, try to solve and then backtrack if it's wrong

### Basic Logic
- For a cell with value v, remove v from the possibilities of the rest of its box, row and column
- If a row, box or column has only a single cell containing the possible value v, then assign v to that cell

These 2 rules are enough to solve basic puzzles. When that fails, more advanced inferences are made.

### Advanced Logic
1. [Omission](https://www.learn-sudoku.com/omission.html): For each box, and for each column or row intersecting this box,
we know they both contain the numbers 1 through 9.
We also know they have 3 cells in common. So the remaining 6 cells must be identical. This means that if a box
cannot contain value v in these 6 cells (the 6 not interesecting the row/column) then the rest of the row/column cannot
contain value v either, and vice versa.

2. [Naked Pairs/Triples/Quads/+](https://www.learn-sudoku.com/naked-pairs.html): Within a box, row or column, if
there exists a subset of $n$ cells with only $n$ possible values within those cells,
then they must contain those $n$ values between them. The remaining $9 - n$ cells cannot contain these $n$ values. In practice,
this requires finding the powerset of the $9$ cells which contains $2^9 = 512$ elements, so still very fast.

### Tree search
This was all I could come up with from my limited Sudoku knowledge but it turns out there are [even more techniques](https://www.learn-sudoku.com/advanced-techniques.html).
However, these techinques give only marginal gains, removing one or two possibilities at a time.
After applying the above logic the possibilities are trimmed down sufficiently that a tree search is very feasible.
At most a single guess is required to solve the [Master puzzles](https://sudoku.com/evil/) on [sudoku.com](sudoku.com).
Even the supposed [hardest Sudoku ever](https://abcnews.go.com/blogs/headlines/2012/06/can-you-solve-the-hardest-ever-sudoku)
is solved in only 0.06 seconds with 14 guesses.
