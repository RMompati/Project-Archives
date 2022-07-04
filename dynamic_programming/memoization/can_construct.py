"""
Write a function `can_construct (target, word_bank)` that accepts a target
string and an array of strings.

The function should return a boolean indicating whether or not the `target` can
be constructed by concatinating elements of the `word_bank` array.

    - You may reuse elements of the `word_bank` as many times as needed.

can_construct(abcdef, [ab, abc, cd, def, adbc]) -> True


m = len(target)
n = len(word_bank)

Brute Force
--

time  : O ( (n ^ m) * m )
space : O ( m ^ 2 )


Memoized
--

time  : O (n * (m ^ 2))
space : O ( m ^ 2 )
"""

def can_construct (target: str, word_bank, memo={}):
    if target in memo.keys():
        return memo.get(target)

    if target == '':
        return True

    for word in word_bank:
        if target.startswith(word) is True:
            suffix = target[len(word):]

            if can_construct(suffix, word_bank, memo) is True:
                memo[target] = True
                return True

    memo[target] = False
    return False


if __name__ == '__main__':
    print(can_construct('abcdef', ['ab', 'abc', 'cd', 'def', 'abcd'], {}))
    print(can_construct('skateboard', ['bo', 'rd', 'ate', 't',\
        'ska', 'sk', 'boar'], {}))
    print(can_construct('enterapotentpot', [
          'a', 'p', 'ent', 'enter', 'ot', 'o', 't'], {}))
    print(can_construct('eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeef', [
        'e', 'ee', 'eee', 'eeee', 'eeeee', 'eeeeee'
    ], {}))
