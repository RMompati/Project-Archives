"""
Write a function `best_sum (target_sum, numbers)` that takes in a target_sum
and an array of numbers as arguments.

The function should return an array containing the shortest combination of
numbers that add up exactly to the target_sum.

    - If there is a tie for the shortest combination, you ,ay return any one of the shortest.


m = target_sum
n = numbers.length

Brute Force
--

time: O( n ^ m * m )
space: O( m ^ 2 )

Memoize
--

time  : O( (m ^ 2) * n )
space : O(  m ^ 2 )


---

can_sum -> Decision Problem.
how_sum -> Combinatoric Problem.
best_sum -> Optimization Problem.

"""


def best_sum (target_sum, numbers, memo={}):

    if target_sum in memo.keys():
        return memo.get(target_sum)

    if target_sum == 0:
        return []

    if target_sum < 0:
        return None

    shortest_combination = None

    for number in numbers:

        remainder = target_sum - number

        results = best_sum(remainder, numbers, memo)

        if results is not None:

            combination = results + [number]

            if shortest_combination is None or  len(combination) < len(shortest_combination):
                shortest_combination = combination

    memo[target_sum] = shortest_combination
    return shortest_combination



if __name__ == '__main__':
    print(best_sum(7, [5, 3, 4, 7], {}))
    print(best_sum(8, [2, 3, 5], {}))
    print(best_sum(8, [1, 4, 5], {}))
    print(best_sum(100, [1, 2, 5, 25], {}))
