"""
Write a function `count_construct (target, word_bank)` that accepts a target
string and an array of strings.

The function  should return the number of ways that the `target` can be
constructed by concatenating elements of the `word_bank` array.

    - You may reuse elements of the `word_bank` as many times as needed.


m = len(target)
n = len(word_bank)

Brute Force                 -->     Memoized
--                                  --

O ( (n ^ m) * m  ) time             O ( n * (m ^ 2) ) time
O ( m ^ 2 ) space                   O ( m ^ 2) space
"""

def count_construct (target: str, word_bank, memo={}):

    if target in memo.keys():
        return memo[target]

    if target == '':
        return 1

    total_count = 0

    for word in word_bank:
        if target.startswith(word) is True:
            suffix = target[len(word):]

            number_of_ways = count_construct(suffix, word_bank, memo)
            total_count += number_of_ways

    memo[target] = total_count
    return total_count


if __name__ == '__main__':
    print(count_construct('purple', ['purp', 'p', 'ur', 'le', 'purpl'], {}))
    print(count_construct('abcdef', ['ab', 'abc', 'cd', 'def', 'abcd'], {}))
    print(count_construct('skateboard', ['bo', 'rd', 'ate', 't',
                                       'ska', 'sk', 'boar'], {}))
    print(count_construct('enterapotentpot', [
          'a', 'p', 'ent', 'enter', 'ot', 'o', 't'], {}))
    print(count_construct('eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeef', [
        'e', 'ee', 'eee', 'eeee', 'eeeee', 'eeeeee'
    ], {}))
