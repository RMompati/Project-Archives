"""
Grid Traveler - Memoization
--

You are a traveler on a 2D grid. You begin in the top-left corner and your goal
is to travel to the bottom-right corner. You may only move down or right.

In how many ways can you travel to the goal on a grid with dimensions m * n?

write a function grid_traveler(m, n) that calculates this.


i.e grid_traveler(2, 3) == 3
    1. Right, Right, Down
    2. Right, Down, Right
    3. Down, Right, Right

i.e grid_traveler(3, 3)
    1.
"""


def grid_traveler(m, n, memo={}):
    if (m, n) in memo.keys():
        return memo[(m, n)]

    if m == 1 and n == 1:
        return 1
    if m == 0 or n == 0:
        return 0

    memo[(m, n)] = grid_traveler(m - 1, n, memo) + \
        grid_traveler(m, n - 1, memo)
    return memo[(m, n)]


if __name__ == '__main__':
    print(grid_traveler(1, 1))
    print(grid_traveler(2, 3))
    print(grid_traveler(3, 2))
    print(grid_traveler(3, 3))
    print(grid_traveler(18, 18))
