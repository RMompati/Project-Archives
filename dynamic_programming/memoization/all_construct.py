"""
Write a function `all_construct (target, word_bank)` that accepts a target
string and array of strings.

The function should return a 2D array concatinating all of the ways that the
`target` can be constructed by concatenating elements of the `word_bank` array.


    - You may reuse elements of the `word_bank` as many times as needed.
"""


def all_construct (target, word_bank, memo={}):

    if target in memo.keys():
        return memo[target]

    if target == '':
        return [[]]

    final_results = []

    for word in word_bank:
        if target.startswith(word):
            suffix = target[len(word):]
            results = all_construct(suffix, word_bank)
            results_ways = list(map((lambda way: [word] + way), results))

            final_results += results_ways

    memo[target] = final_results
    return final_results


if __name__ == '__main__':
    print(all_construct(
        'purple', ['purp', 'p', 'ur', 'le', 'purpl'], {}))
    print(all_construct(
        'abcdef', ['ab', 'abc', 'cd', 'def', 'abcd', 'ef', 'c'], {}))
    print(all_construct(
        'skateboard', ['bo', 'rd', 'ate', 't', 'ska', 'sk', 'boar'], {}))
    print(all_construct(
        'aaaaaaaaaaaaaaaaaaaaaaaaaaz', ['a', 'aaa', 'aaa', 'aaaa', 'aaaaa'], {}))
