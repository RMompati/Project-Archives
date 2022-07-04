"""
Write a function `can_sum (target_sum, numbers)` that takes in a target sum and
an array if numbers as arguments.

The function should return a boolean indicating whether or not it is possible
to generate the target_sum using numbers from the array.

    - You may use an element of the array as many times as needed.

    - You may assume that all input numbers are non-negatives.


FAILED
"""


def can_sum(target_sum, numbers, memo={}):

    if target_sum in memo.keys():
        return memo.get(target_sum)

    if target_sum == 0:
        return 1

    if target_sum < 0:
        return 0

    for number in numbers:
        remainder = target_sum - number

        if can_sum(remainder, numbers, memo) == 1:
            memo[remainder] = 1
            return 1

    memo[target_sum] = 0
    return 0


if __name__ == "__main__":
    print(True if can_sum(7, [2, 3], {}) == 1 else False)
    print(True if can_sum(7, [5, 3, 4, 7], {}) == 1 else False)
    print(True if can_sum(7, [2, 4], {}) == 1 else False)
    print(True if can_sum(8, [2, 3, 5], {}) == 1 else False)
    print(True if can_sum(300, [7, 14], {}) == 1 else False)
