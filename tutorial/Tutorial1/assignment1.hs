-- Description: assignment1 for COMP3259
-- Author: Chengsheng SHEN
-- Date: 10/09/2015

module Tutorial1 where

import Data.Char (ord)
import Prelude hiding (Maybe(..))



absolute :: Int -> Int
absolute x = if x < 0 then -x else x

-- Question 1:
nested_if1 x = if ( (absolute x) <= 10)
                then x
                else error "Only numbers between [-10, 10] allowed"

nested_if2 x = if ( (if x < 0 then -x else x) <= 10)
                then x
                else error "Only numbers between [-10, 10] allowed"


identity x = x

--Question 4
tl :: [a] -> [a]
tl [] = []
tl (x:xs) = xs

-- | Factorial function
--
-- Examples:
--
-- >>> factorial 3
-- 6
--
-- >>> factorial 0
-- 1
factorial :: Int -> Int
factorial 0 = 1
factorial x = factorial (x - 1) * x

-- | Compute Fibonacci numbers
--
-- Examples:
--
-- >>> fibonacci 10
-- 55
--
-- >>> fibonacci 5
-- 5
fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)


-- |
-- >>> mapList absolute [4,-5,9,-7]
-- [4,5,9,7]
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs



-- |
-- >>> ascii "abcds"
-- [97,98,99,100,115]
ascii :: [Char] -> [Int]
ascii [] = []
ascii (x:xs) = ord x : ascii xs



-- |
-- >>> filterList even [1,2,3,4,5]
-- [2,4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList f (x:xs)
    | f x = x : filterList f xs
    | otherwise = filterList f xs


-- |
-- >>> zipList [1,2,3] ['a', 'b', 'c']
-- [(1,'a'),(2,'b'),(3,'c')]
zipList :: [a] -> [b] -> [(a, b)]
zipList _ [] = []
zipList [] _ = []
zipList (x:xs) (y:ys) = (x, y) : zipList xs ys



-- |
-- >>> zipSum [1,2,3] [4,5,6]
-- [5,7,9]
zipSum :: [Int] -> [Int] -> [Int]
zipSum _ [] = []
zipSum [] _ = []
zipSum (x:xs) (y:ys) = (x + y) : zipSum xs ys


data Maybe a = Nothing | Just a deriving Show

-- |
-- >>> safeHead []
-- Nothing
--
-- >>> safeHead [1,2,3]
-- Just 1
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


-- |
-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1,2]
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = 
    case x of {
        Nothing -> catMaybes xs;
        Just n -> n : catMaybes xs
        }