# Dynamic Programming

In here I solve some dynamic programming questions following [this](https://www.youtube.com/watch?v=oBt53YbR9Kk) tutorial.

## Memoization solutions

#### The Memoization Recipe
1. Make it work:
    - Visualize the problem as tree.
    - Implement the tree using recursion: The Brute Force Solution.
    - Test it.


2. Make it efficient.
    - Add a memoization object.
    - Add a base case to return the memoized values.
    - Memoize the return values.
---
---
#### Problem 1
> Write a function `all_construct (target, word_bank)` that accepts a target
string and array of strings.
The function should return a 2D array concatinating all of the ways that the
`target` can be constructed by concatenating elements of the `word_bank` array.
- You may reuse elements of the `word_bank` as many times as needed.

#### Solution:
> The solution is in ```memoization/all_construct.py```
---

#### Problem 2
> Write a function `best_sum (target_sum, numbers)` that takes in a target_sum
and an array of numbers as arguments.
 The function should return an array containing the shortest combination of
numbers that add up exactly to the target_sum.
 If there is a tie for the shortest combination, you ,ay return any one of the shortest.

#### Solution:
> The solution is in ```memoization/best_sum.py```
---

#### Problem 3
> Write a function `can_construct (target, word_bank)` that accepts a target
string and an array of strings.
The function should return a boolean indicating whether or not the `target` can
be constructed by concatinating elements of the `word_bank` array.
You may reuse elements of the `word_bank` as many times as needed.
**Example**: ```can_construct(abcdef, [ab, abc, cd, def, adbc]) -> True```

#### Solution:
> The solution is in ```memoization/can_construct.py```
---

#### Problem 4
> Write a function `can_sum (target_sum, numbers)` that takes in a target sum and
an array if numbers as arguments.
The function should return a boolean indicating whether or not it is possible
to generate the target_sum using numbers from the array.
- You may use an element of the array as many times as needed.
- You may assume that all input numbers are non-negatives.

#### Solution:
> The solution is in ```memoization/can_sum.py```
---

#### Problem 5
> Write a function `count_construct (target, word_bank)` that accepts a target
string and an array of strings.
The function  should return the number of ways that the `target` can be
constructed by concatenating elements of the `word_bank` array.
- You may reuse elements of the `word_bank` as many times as needed.

#### Solution:
> The solution is in ```memoization/count_construct.py```
---

#### Problem 6
> Write a function `fib (n)` that takes in number as an argument. The function
should return the n-th number of the Fibonacci sequence.
- The ``0th`` number of the sequence is ``0``.
- The ``1st`` number of the sequence is ``1``.
>To generate the next number of the sequence, we sum the previous two.

#### Solution:
> The solution is in ```memoization/fibonacci.py```
---

#### Problem 7
> Grid Traveler - Memoization
You are a traveler on a 2D grid. You begin in the top-left corner and your goal
is to travel to the bottom-right corner. You may only move down or right.
In how many ways can you travel to the goal on a grid with dimensions m * n?
write a function grid_traveler(m, n) that calculates this.
- i.e grid_traveler(2, 3) == 3
    1. Right, Right, Down
    2. Right, Down, Right
    3. Down, Right, Right
#### Solution:
> The solution is in ```memoization/grid_traveler.py```
---

#### Problem 8
> Write a function `how_sum (target_sum, numbers)` that takes in a target_sum and
an array of numbers as arguments.
The function should return an array containing any combinations of elements
that add up to the target_sum. If there is no combination that adds up to the
target_sum, then return null.
- If there are multiple combinations possible, you may return any single one.

#### Solution:
> The solution is in ```memoization/how_sum.py```
---
---

## Tabulation solutions
#### The Tabulation Recipe

- Visualize the problem as a table.
- Size the table based on the inputs.
- initialize the table with base cases.
- Seed the trivial answer into the table.
- Iterate through the table.
- Fill further positions based of the current position.
---
---

#### Problem 4
> ... The problem is defined above.

#### Solution:
> The solution is in ```tabulation/can_sum.py```
---

#### Problem 6
> ... The problem is defined above.

#### Solution:
> The solution is in ```tabulation/fibonacci.py```
---

#### Problem 7
> ... The problem is defined above.
#### Solution:
> The solution is in ```tabulation/grid_traveler.py```
---

Happy hacking...