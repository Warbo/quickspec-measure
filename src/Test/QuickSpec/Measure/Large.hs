{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module Test.QuickSpec.Measure.Large where

import Data.Typeable
import Test.QuickSpec
import Test.QuickCheck
import Data.Array
import qualified Data.List as L
import Data.Ord

-- Arrays

put :: Ix i => i -> a -> Array i a -> Array i a
put ix v arr = arr // [(ix, v)]

genRange :: Gen (Int, Int)
genRange = do
  low <- choose (-2, 2)
  high <- fmap (low +) (choose (-1, 2))
  return (low, high)

instance Arbitrary a => Arbitrary (Array Int a) where
  arbitrary = do
    (low, high) <- genRange
    elems <- arbitrary :: Gen (Int -> Maybe a)
    return (array (low, high) [(i, x) | i <- [low..high], Just x <- [elems i]])

-- Heaps

data Heap a = Nil | Branch Int a (Heap a) (Heap a) deriving Typeable

instance Ord a => Eq (Heap a) where
  h1 == h2 = toList h1 == toList h2

instance Ord a => Ord (Heap a) where
  compare = comparing toList

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = fmap fromList arbitrary

toList :: Ord a => Heap a -> [a]
toList h | hNull h = []
         | otherwise = findMin h:toList (deleteMin h)

fromList :: Ord a => [a] -> Heap a
fromList = foldr insert Nil

hNull :: Heap a -> Bool
hNull Nil = True
hNull _ = False

findMin :: Heap a -> a
findMin (Branch _ x _ _) = x

insert :: Ord a => a -> Heap a -> Heap a
insert x h = merge h (branch x Nil Nil)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Branch _ _ l r) = merge l r

branch :: Ord a => a -> Heap a -> Heap a -> Heap a
branch x l r | npl l <= npl r = Branch (npl l + 1) x l r
             | otherwise = Branch (npl r + 1) x r l

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Nil h = h
merge h Nil = h
merge h1@(Branch _ x1 l1 r1) h2@(Branch _ x2 l2 r2)
 | x1 <= x2 = branch x1 (merge l1 h2) r1
 | otherwise = merge h2 h1

npl :: Heap a -> Int
npl Nil = 0
npl (Branch n _ _ _) = n

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists [] xs = xs
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys)
  | x < y = x:mergeLists xs (y:ys)
  | otherwise = y:mergeLists (x:xs) ys

-- Theory

functions :: [Sig]
functions = [
    "0" `fun0` (0   :: Int),
    "1" `fun0` (1   :: Int),
    "+" `fun2` ((+) :: Int -> Int -> Int),
    "*" `fun2` ((*) :: Int -> Int -> Int),
    "!"         `fun2` ((!)       :: Array Int Int -> Int -> Int),
    "put"       `fun3` (put       :: Int -> Int -> Array Int Int -> Array Int Int),
    "listArray" `fun2` (listArray :: (Int, Int) -> [Int] -> Array Int Int),
    "elems"     `fun1` (elems     :: Array Int Int -> [Int]),
    "indices"   `fun1` (indices   :: Array Int Int -> [Int]),
    "||"    `fun2` (||),
    "&&"    `fun2` (&&),
    "not"   `fun1` not,
    "True"  `fun0` True,
    "False" `fun0` False,
    blind2 "."   ((.) :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)),

  -- Similarly, id is not treated as a function.
  blind0 "id"  (id  :: Int -> Int),

  -- Tell QuickSpec how to compare values of function type:
  -- i.e., generate a random argument and apply the function to it.
  observer2 $ \x (f :: Int -> Int) -> f x,

    "nil"        `fun0` (Nil        :: Heap Int),
  "insert"     `fun2` (insert     :: Int -> Heap Int -> Heap Int),
  "findMin"    `fun1` (findMin    :: Heap Int -> Int),
  "deleteMin"  `fun1` (deleteMin  :: Heap Int -> Heap Int),
  "merge"      `fun2` (merge      :: Heap Int -> Heap Int -> Heap Int),
  "hNull"       `fun1` (hNull       :: Heap Int -> Bool),
  "fromList"   `fun1` (fromList   :: [Int] -> Heap Int),

  "toList"     `fun1` (toList     :: Heap Int -> [Int]),
  "sort"       `fun1` (L.sort     :: [Int] -> [Int]),
  "insertList" `fun2` (L.insert   :: Int -> [Int] -> [Int]),
  "nullList"   `fun1` (L.null     :: [Int] -> Bool),
  "deleteList" `fun2` (L.delete   :: Int -> [Int] -> [Int]),
  "mergeLists" `fun2` (mergeLists :: [Int] -> [Int] -> [Int]),
  "unit"    `fun1` (return  :: Int -> [Int]),
  -- Don't take ++ from the prelude because we want to see laws about it
  "++"      `fun2` ((++)    :: [Int] -> [Int] -> [Int]),
  "length"  `fun1` (length  :: [Int] -> Int),
  "reverse" `fun1` (reverse :: [Int] -> [Int]),
  "map"     `fun2` (map     :: (Int -> Int) -> [Int] -> [Int])
  ]

variables :: [Sig]
variables = [
    ["i1", "i2", "i3"] `vars` (undefined :: Int),
    ["a1"]             `vars` (undefined :: Array Int Int),
    ["b1", "b2", "b3"] `vars` (undefined :: Bool),
    vars ["f1", "f2", "f3"] (undefined :: Int -> Int),
    ["h1", "h2", "h3"] `vars` (undefined :: Heap Int)
  ]
