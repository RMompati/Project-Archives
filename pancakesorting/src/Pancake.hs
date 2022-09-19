{-|
    Author: Mompati Erold Keetile
-}

module Pancake     (
    applyFlipSequence, flip', flipSequence,
    isSorted, optFlipSequence,
    maxLength, maxOptLength, desc) where

import Data.List (maximumBy, permutations)
import Data.Ord (comparing)

{-|
    A simple project description function 'desc'.
-}

desc :: IO ()
desc = putStrLn "\nImplementation of the Pancake Sorting algorithm using Haskell!\n"

-- The utility function(s).

{-|
    maxIndex returns the index of the maximum element of [a].

    Taken Here: <https://stackoverflow.com/questions/1496980/finding-index-of-element-in-a-list-in-haskell>
-}

maxIndex :: Ord a => [a] -> Int
maxIndex = fst . maximumBy (comparing snd) . zip [0..]


-- The beginning of the assignment.

{-|

    1. flip' i reverses the list of the first (i + 1) elements of the list [a]
        and leave the remainder of the list unchanged.
-}

flip' :: Int -> [a] -> [a]
flip' _ []  = []
flip' i xs  = reverse (take (i + 1) xs) ++ drop (i + 1) xs


{- |

    2. flipSequence : Returns a list of flips as Ints required to archive a
        sort of [a] using the Pancake Algorithm.
-}

flipSequence :: Ord a => [a] -> [Int]
flipSequence xs
    | isSorted xs       = []
    | otherwise         = aux xs []
    where
        aux ws fs
            | length ws == 1    = fs
            | otherwise         = aux zs (fs ++ [i])
            where
                i   = maxIndex ws
                zs  = init $ flip' (length ws) $ flip' i ws

{- |

    3. optFlipSequence : Returns a list of flips of minimal length that sorts
        [a].
-}

optFlipSequence :: Ord a => [a] -> [Int]
optFlipSequence xs
    | isSorted xs       = []
    | otherwise         = aux (length xs - 1) xs []
    where
        aux xslen ys fs
            | isSorted ys   = fs
            | xslen == 0    = fs
            | otherwise     = aux (xslen - 1) (flip' xslen $ flip' i ys) (fs ++ [i])
            where
                i = maxIndex $ take (xslen + 1) ys

{- |

    4. isSorted : Returns True if [a] is sorted, otherwise False.
-}

isSorted :: Ord a => [a] -> Bool
isSorted []             = True
isSorted [x]            = True
isSorted (x : y : xs)   = x <= y && isSorted (y : xs)


{- |

    5. applyFlipSequence : Apply flip consecutively to the the first [a] to
        produce the second [a] using consecutive flips in [Int].
-}

applyFlipSequence :: [Int] -> [a] -> [a]
applyFlipSequence [] _  = []
applyFlipSequence _ []  = []
applyFlipSequence fs xs = aux xslen fs xs
    where
        xslen = length xs - 1

        aux _ [] []         = []
        aux _ [] (_ : _)    = []
        aux yslen (s : fs) ys
            | null fs    = flip' yslen $ flip' s ys
            | yslen /= s        = aux (yslen - 1) fs $ flip' yslen $ flip' s ys
            | otherwise         = aux (yslen - 1) fs ys

{- |

    6. maxLength is the maximum length of 'flipSequence xs', when length of xs
        is equal to n.
-}

maxLength :: Int -> Int
maxLength n
    | n <= 1    = 0
    | otherwise =
        maximum $ map length $
        [flipSequence xs | xs <- tail $ permutations [1 .. n]]

{- |

    7. maxOptLength is the maximum length of 'optFlipSequence xs', when length
        of xs is equal to n.
-}

maxOptLength :: Int -> Int
maxOptLength n
    | n <= 1    = 0
    | otherwise =
        maximum $ map length $
            [optFlipSequence xs | xs <- tail $ permutations [1 .. n]]
