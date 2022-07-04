"""
Can Sum - Tabulation
"""

def can_sum (target, numbers):
    table = [False for _ in range(target + 1)]
    table[0] = True

    for i in range(len(table)):
        if table[i] is True:
            for number in numbers:
                if i + number <= target:
                    table[i + number] = True

    return table[target]


if __name__ == '__main__':
    print(can_sum(7, [5, 3, 4]))
    print(can_sum(7, [5, 3, 4, 7]))
    print(can_sum(7, [2, 4]))
    print(can_sum(8, [2, 3, 5]))
    print(can_sum(300, [7, 14]))
