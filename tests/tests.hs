{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}


import Test.QuickCheck
-- import Test.QuickCheck.All
-- import System.Exit (exitSuccess,exitFailure)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)


import Calculational
import qualified Data.Set as S
import qualified Data.MultiSet as MS
import qualified Data.List as L

prop_Expr1 :: Int -> Property
prop_Expr1 n = n >= 0 ==> [calc| length . take n . map (\x -> x * x) $ [0..] |] == n

prop_Expr2 :: Integer -> Integer -> Integer -> Bool
prop_Expr2 x1 x2 x3 = (last . L.sort) [x1,x2,x3] == [calc| x1 ↑ x2 ↑ x3 |]

prop_Expr3 :: Integer -> Integer -> Integer -> Bool
prop_Expr3 x1 x2 x3 = (head . L.sort) [x1,x2,x3] == [calc| x1 ↓ x2 ↓ x3 |]

prop_Expr4 :: Integer -> Integer -> Integer -> Bool
prop_Expr4 x1 x2 x3 = [calc| x1 ↓ (x2 ↑ x3) |] == [calc| (x1 ↓ x2) ↑ (x1 ↓ x3) |]

prop_Expr5 :: Integer -> Integer -> Integer -> Bool
prop_Expr5 x1 x2 x3 = [calc| x1 + (x2 ↑ x3) |] == [calc| (x1 + x2) ↑ (x1 + x3) |]

prop_Expr6 :: Integer -> Integer -> Integer -> Bool
prop_Expr6 x1 x2 x3 = [calc| x1 + (x2 ↓ x3) |] == [calc| (x1 + x2) ↓ (x1 + x3) |]

prop_Expr7 :: [Integer] -> [Integer] -> [Integer] -> Bool
prop_Expr7 x1 x2 x3 = [calc| x1 ∪ (x2 ∩ x3) |] == [calc| (x1 ∪ x2) ∩ (x1 ∪ x3) |]

prop_Expr8 :: [Integer] -> [Integer] -> [Integer] -> Bool
prop_Expr8 x1 x2 x3 = (L.sort . L.nub) [calc| x1 ∩ (x2 ∪ x3) |] == (L.sort . L.nub) [calc| (x1 ∩ x2) ∪ (x1 ∩ x3) |]

prop_Expr9 :: [Integer] -> [Integer] -> [Integer] -> Bool
prop_Expr9 x1 x2 x3 =
  [calc| S.fromList x1 ∩ ( S.fromList x2 ∪  S.fromList x3) |]
  == [calc| ( S.fromList x1 ∩  S.fromList x2) ∪ ( S.fromList x1 ∩  S.fromList x3) |]

prop_Expr10 :: [Integer] -> [Integer] -> [Integer] -> Bool
prop_Expr10 x1 x2 x3 =
  MS.toSet [calc| MS.fromList x1 ∩ ( MS.fromList x2 ∪  MS.fromList x3) |]
  == MS.toSet [calc| ( MS.fromList x1 ∩  MS.fromList x2) ∪ ( MS.fromList x1 ∩ MS.fromList x3) |]

prop_Expr11 :: [Integer] -> [Integer] -> [Integer] -> Bool
prop_Expr11 x1 x2 x3 =
  [calc| S.fromList x1 ∩ ( S.fromList x2 ∪  S.fromList x3) |]
  == [calc| ( S.fromList x1 ∩  S.fromList x2) ∪ ( S.fromList x1 ∩ S.fromList x3) |]

prop_model_sum1 :: [Integer] -> Bool
prop_model_sum1 xs = sum xs == [calc| (+ x <- xs : : x) |] 

prop_model_sum2 :: [Integer] -> Bool
prop_model_sum2 xs = (sum . filter (\x -> 0 <= x && x <= 10) $ xs)
                     == [calc| (+ x <- xs : 0 <= x <= 10 : x) |] 

prop_model_prod1 :: [Integer] -> Bool
prop_model_prod1 xs = product xs == [calc| (* x <- xs : : x) |] 

prop_model_prod2 :: [Integer] -> Bool
prop_model_prod2 xs = (product . filter (\x -> 0 <= x && x <= 10) $ xs)
                     == [calc| (* x <- xs : 0 <= x <= 10 : x) |] 

prop_model_max1 :: [Integer] -> Property
prop_model_max1 xs = xs /= [] ==>
  (maximum . map Value $ xs) == [calc| (↑ x <- xs : : Value x) |] 

prop_model_max2 :: Integer -> Integer -> [Integer] -> Property
prop_model_max2 a b xs = [ x | x <- xs, a <= x && x <= b] /= [] ==>
  maximum [Value x | x <- xs, a <= x && x <= b]
                             == [calc| (↑ x <- xs : a <= x <= b : Value x) |] 

prop_model_max3 :: Integer -> Integer -> [Integer] -> Property
prop_model_max3 a b xs = [ x | x <- xs, a <= x && x <= b] /= [] ==>
  maximum (map (Value . (^2)) . filter (\x -> a <= x && x <= b) $ xs)
          == [calc| (↑ x <- xs : a <= x <= b : Value (x^2)) |] 

prop_model_min1 :: [Integer] -> Property
prop_model_min1 xs = xs /= [] ==>
  (minimum . map Value $ xs) == [calc| (↓ x <- xs : : Value x) |] 

prop_model_min2 :: Integer -> Integer -> [Integer] -> Property
prop_model_min2 a b xs = [ x | x <- xs, a <= x && x <= b] /= [] ==>
  minimum [x | x <- xs, a <= x && x <= b]
          == [calc| getValue (↓ x <- xs : a <= x <= b : Value x) |] 

prop_model_min3 :: Integer -> Integer -> [Integer] -> Property
prop_model_min3 a b xs = [ x | x <- xs, a <= x && x <= b] /= [] ==>
  minimum (map (^2) . filter (\x -> a <= x && x <= b) $ xs)
          == [calc| getValue (↓ x <- xs : a <= x <= b : Value (x^2)) |] 


prop_model_list1 :: Integer -> Integer -> Bool
prop_model_list1 x y = [x .. y] == [calc| [ x .. y ] |]

prop_model_list2 :: Integer -> Integer -> Integer -> Property
prop_model_list2 x y z = x /= y ==> [ x,y .. z] == [calc| [ x,y .. z ] |]

prop_model_list3 :: Integer -> [Integer] -> Bool
prop_model_list3 x xs = (x `elem` xs) == [calc| x ∊ xs |]

prop_model_list4 :: [Integer] -> [Integer] -> Bool
prop_model_list4 xs ys = (xs `L.intersect` ys == xs) == [calc| xs ⊆ ys |]

prop_model_list5 :: [Integer] -> [Integer] -> Bool
prop_model_list5 xs ys = xs `L.union` ys == [calc| xs ⋃ ys |]

prop_model_list6 :: [[Integer]] -> Bool
prop_model_list6 xss = (foldr L.union [] xss) == [calc| (⋃ xs <- xss : : xs) |]

prop_model_list7 :: [Integer] -> [Integer] -> Bool
prop_model_list7 xs ys = xs `L.intersect` ys == [calc| xs ⋂ ys |]

prop_model_list8 :: [[Integer]] -> Property
prop_model_list8 xss =
  xss /= [] ==> (foldr1 L.intersect xss)
                == getContainer [calc| (⋂ xs <- xss : : Container xs) |]

prop_model_list9 :: [[Integer]] -> Bool
prop_model_list9 xss =
  Universe == [calc| (⋂ xs <- xss : False : Container xs) |]

prop_model_set1 :: Integer -> Integer -> Bool
prop_model_set1 x y = S.fromList [x .. y] == [calc| { x .. y } |]

prop_model_set2 :: Integer -> Integer -> Integer -> Property
prop_model_set2 x y z = x /= y ==> S.fromList [ x,y .. z]
                                      == [calc| { x,y .. z } |]
prop_model_set3 :: [Integer] -> Bool
prop_model_set3 xs = S.fromList (map (^2) xs) == [calc| { x <- xs : : x^2 } |]

prop_model_mset1 :: [Integer] -> Bool
prop_model_mset1 xs = MS.fromList (map (^2) xs)
                          == [calc| {| x <- xs : : x^2 |} |]

prop_model_mset2 :: Integer -> Integer -> Integer -> Property
prop_model_mset2 x y z = (x /= y) ==> MS.fromList [ x,y .. z]
                                      == [calc| {| x,y .. z |} |]

prop_model_mset3 :: [Integer] -> Bool
prop_model_mset3 xs = MS.fromList (map (^2)
                                   . filter (\x -> 0 <= x && x <= 10) $ xs)
                          == [calc| {| x <- xs : 0 <= x <= 10 : x^2 |} |]


-- return []
-- runTests = $(quickCheckAll)

-- main :: IO ()
-- main = $(quickCheckAll)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
    [ testGroup "haskell expressions"
      [ testProperty "dot/dollar/hof" prop_Expr1
      ]
    , testGroup "min/max expressions"  
      [ testProperty "max" prop_Expr2
      , testProperty "min" prop_Expr3
      , testProperty "min/max" prop_Expr4
      , testProperty "add/max" prop_Expr5
      , testProperty "add/min" prop_Expr6
      ]
    , testGroup "union/intersection expressions"  
      [ testProperty "lists/union/intersect" prop_Expr7
      , testProperty "lists/intersect/union" prop_Expr8
      , testProperty "multiset/intersect/union" prop_Expr9
      , testProperty "multiset/intersect/union" prop_Expr11
      ]
    , testGroup "quantifier expressions"  
      [ testProperty "sum" prop_model_sum1
      , testProperty "sum/filter" prop_model_sum2
      , testProperty "prod" prop_model_prod1      
      , testProperty "prod/filter" prop_model_prod2
      , testProperty "Max" prop_model_max1
      , testProperty "Max/filter" prop_model_max2
      , testProperty "Max/filter/body" prop_model_max3
      , testProperty "Min" prop_model_min1
      , testProperty "Min/filter" prop_model_min2
      , testProperty "Min/filter/body" prop_model_min3
      ]
    , testGroup "list expressions"  
      [ testProperty "range" prop_model_list1
      , testProperty "range/step" prop_model_list2
      , testProperty "elem" prop_model_list3
      , testProperty "sublist" prop_model_list4
      , testProperty "list/union" prop_model_list5
      , testProperty "list/Union/quantifier" prop_model_list6
      , testProperty "list/intersect" prop_model_list7
      , testProperty "list/intersect/quantifier" prop_model_list8
      , testProperty "list/empty/intersect/quantifier" prop_model_list9
      ]
    , testGroup "set expressions"  
      [ testProperty "set/range" prop_model_set1
      , testProperty "set/range/step" prop_model_set2
      , testProperty "set/comprehension" prop_model_set3
      ]
    , testGroup "multiset expressions"  
      [ testProperty "mset/range" prop_model_mset1
      , testProperty "mset/range/step" prop_model_mset2
      , testProperty "mset/comprehension" prop_model_mset3
      ]
    ]
