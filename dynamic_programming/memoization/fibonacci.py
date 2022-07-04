"""
Dynamic Programming Problems: Memoizatio - Fibonacci Sequence
"""

def classic_fib(n):
    return 1 if n <= 2 else fib (n - 1) + fib (n - 2)



def fib(n, memo={}):
    """
    Computes the nth value in the fibonacci sequence, using memoization
    """
    if n in memo.keys():
        return memo.get(n)

    memo[n] = 1 if n <= 2 else fib(n - 1, memo) + fib(n - 2, memo)

    return memo[n]


if __name__ == '__main__':
    print(fib(6))
    print(fib(7))
    print(fib(8))
    print(fib(50))
