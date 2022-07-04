"""
Write a function `fib (n)` that takes in number as an argument. The function
should return the n-th number of the Fibonacci sequence.


    - The 0th number of the sequence is 0.
    - The 1st number of the sequence is 1.

To generate the next number of the sequence, we sum the previous two.
"""

def fib (n):
    table = [0 for x in range(n + 1)]
    table[1] = 1
    for i in range(n):
        try:
            table[i + 1] += table[i]
            table[i + 2] += table[i]
        except IndexError:
            pass

    return table[n]


if __name__ == '__main__':
    print(fib(6))
    print(fib(7))
    print(fib(8))
    print(fib(50))
