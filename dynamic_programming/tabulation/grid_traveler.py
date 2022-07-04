"""
Grid Traveler - Tabulation
"""
def grid_traveler(m, n):
    table = [[0 for _ in range(n + 1)] for _ in range(m + 1)]
    table[1][1] = 1

    for r in range(m + 1):
        for c in range(n + 1):
            if r != m:
                table[r + 1][c] += table[r][c]

            if c != n:
                table[r][c + 1] += table[r][c]



    return table[m][n]


if __name__ == '__main__':
    print((grid_traveler(1, 1)))
    print((grid_traveler(2, 3)))
    print((grid_traveler(3, 2)))
    print((grid_traveler(3, 3)))
    print(grid_traveler(18, 18))
