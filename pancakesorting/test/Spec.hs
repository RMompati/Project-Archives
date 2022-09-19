{-|
    Author: Mompati Erold Keetile
-}

import           Control.Monad (forM_, when)
import           Data.List     (permutations, sort)
import           Test.Hspec

import           Pancake       (applyFlipSequence, flip', flipSequence,
                                isSorted, optFlipSequence,
                                maxLength, maxOptLength)

main :: IO ()
main =
  hspec $
  parallel $ do
    let n = 10
        m = 8
    describe "All" $ do
      describe "Required" $ do
        forM_ [0 .. n] $ \k -> do
          describe ("k = " ++ show k) $ do
            when (k /= 0) $ testFlip k
            testIsSorted k
            testFlipSequence k
            testApply k m
            when (k <= m) $ testOptFlipSequence k
            when (k < 9) $ testMaxLength k
            when (k < 9) $ testMaxOptLength k
      additionalTests

{-
Run a test across every permutation of [1..k]. This is faster than storing the
evaluated permutations.
-}
check :: Int -> String -> ([Int] -> Expectation) -> Spec
check k label = parallel . it label . forM_ (permutations [1 .. k])

{-
  For all n! permutations of [1..n]:

      (a) isSorted $ applyFlipSequence (flipSequence xs) xs == True
      (b) isSorted $ applyFlipSequence (optFlipSequence xs) xs == True
-}
testApply :: Int -> Int -> Spec
testApply k m =
  parallel $
  describe "Pancake.applyFlipSequence" $ do
    check k "sorts using flipSequence" $ sortsWith flipSequence
    when (k <= m) $
      check k "sorts using optFlipSequence" $ sortsWith optFlipSequence
  where
    sortsWith f xs = applyFlipSequence (f xs) xs `shouldSatisfy` isSorted

{-
For all n! permutations of [1..n]:

  (a) `flip 0 xs == xs` and `flip 1 xs == xs`
  (b) `flip i xs /= xs` for 2 <= i <= n
  (c) `flip i . flip i` == id

-}
testFlip :: Int -> Spec
testFlip k =
  parallel $
  describe "Pancake.flip" $ do
    check k "does not change the input list when i == 0" $ \xs -> do
      flip' 0 xs `shouldBe` xs
    check k "changes the input list when i > 0" $ \xs ->
      forM_ [1 .. k - 1] $ \i -> flip' i xs `shouldSatisfy` (/= xs)
    check k "has no effect when applied twice" $ \xs ->
      forM_ [1 .. k - 1] $ \i -> (flip' i . flip' i) xs `shouldBe` xs

{-
For all n! permutations of [1..n]:

    (a) flipSequence [1,..,n] == []
    (b) length $ flipSequence xs <= 2n -3 (n > 1)

-}
testFlipSequence :: Int -> Spec
testFlipSequence k =
  parallel $
  describe "Pancake.flipSequence" $ do
    it "should not make any flips on a sorted list" $
      flipSequence [1 .. k] `shouldBe` []
    when (k > 1) $
      check k "makes 2n - 3 or fewer flips" $ \xs ->
        length (flipSequence xs) `shouldSatisfy` (<= 2 * k - 3)

{-
For all n! permutations of [1..n]:

    (a) length $ optFlipSequence xs <= length $ flipSequence xs
    (b) optFlipSequence xs <= 18/11 n

-}
testOptFlipSequence :: Int -> Spec
testOptFlipSequence k =
  parallel $
  describe "Pancake.optFlipSequence" $
  check k "uses fewer flips than flipSequence && makes <= 18/11n flips" $ \xs -> do
    let optLen = length $! (optFlipSequence xs)
    optLen `shouldSatisfy` (<= length (flipSequence xs))
    when (k > 1) $
      (fromIntegral optLen :: Double) `shouldSatisfy`
      (<= (18 / 11) * fromIntegral k)

{-
For all n! permutations of [1..n]: 'isSorted xs' <==> 'xs == [1..n]'
-}
testIsSorted :: Int -> Spec
testIsSorted 0 = return ()
testIsSorted k =
  parallel $
  let unsorted = tail $ permutations [1 .. k]
  in describe "isSorted" $
     it "returns True if and only if the input is sorted" $ do
       [1 .. k] `shouldSatisfy` isSorted
       forM_ unsorted $ \xs -> xs `shouldSatisfy` (not . isSorted)

{-
  Maximum length of a list of flips.

  (a). Any flip sequence should be of length <= 2n - 3,
        when n is length of xs

-}
testMaxLength :: Int -> Spec
testMaxLength k =
  parallel $
    describe "Pancake.maxLength" $ do
      when (k > 1) $
        it "returns a value <= 2n - 3" $
          maxLength k `shouldSatisfy` (<= 2 * k - 3)

{-
  Maximum length of a list of flips.

  (a). Any flip sequence should be of length <= 2n - 3,
        when n is length of xs

-}
testMaxOptLength :: Int -> Spec
testMaxOptLength k =
  parallel $
    describe "Pancake.maxOptLength" $ do
      when (k > 1) $
        it "returns a value <= 2n - 3" $
          maxOptLength k `shouldSatisfy` (<= 2 * k - 3)
      when (k > 1) $
        it "returns a value <= 18/11n" $
          (fromIntegral (maxOptLength k) :: Double)
            `shouldSatisfy` (<= (18 / 11) * fromIntegral k)
      when (k > 2) $
        it "returns a value <= 15/14n" $
            (fromIntegral (maxOptLength k) :: Double) `shouldSatisfy`
              (<= (15 / 14) * fromIntegral k)

additionalTests :: Spec
additionalTests =
  describe "Additional" $ do
    it "flip' i xs == reverse start ++ end" $ do
      let xs = [1..30 :: Int]
      forM_ [0..29] $ \i ->
        flip' i xs `shouldBe` reverse (take (i+1) xs) ++ drop (i+1) xs
    it "optFlipSequence sorts a list" $ do
      let xs = "qwertyuiopasdfghjklzxcvbnm"
      applyFlipSequence (optFlipSequence xs) xs `shouldBe` sort xs
    it "flipSequence sorts a list" $ do
      let xs = "qwertyuiopasdfghjklzxcvbnm"
      applyFlipSequence (flipSequence xs) xs `shouldBe` sort xs
    it "Empty list is sorted and has an empty flip sequence with all algorithms" $ do
      let e = [] :: [Int]
      e `shouldSatisfy` isSorted
      flipSequence e `shouldBe` []
      optFlipSequence e `shouldBe` []
