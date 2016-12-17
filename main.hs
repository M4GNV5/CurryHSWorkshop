module Main where

import Data.Char

task4a x			= map isLower x
task4b x			= (and . task4a) x
task4c x			= length (filter isLower x)

fizzBuzzFilter :: Integer -> String
fizzBuzzFilter x
	| mod x 15 == 0	= "fizz buzz"
	| mod x 5 == 0	= "buzz"
	| mod x 3 == 0	= "fizz"
	| otherwise		= show x

fizzbuzz :: [String]
fizzbuzz 			=	map fizzBuzzFilter [1..]

getGreater x y		= 	if x > y
							then x
							else y
maximum' x			=	foldl getGreater 0 x

{-
fib :: Integer -> Integer
fib 0				=	0
fib 1				=	1
fib	n				=	(+) (fib (n - 1)) (fib (n - 2))
-}

fibs				= 	0 : 1 : zipWith (+) (fibs) (tail fibs)
fib	n				=	fibs !! n

collNext :: Integer -> Integer
collNext x
	| even x	= div x 2
	| otherwise	= 3 * x + 1

collSeq x			= (++ [1]) (takeWhile (> 1) (iterate collNext x))
collTest x			= (last (collSeq x)) == 1

encode :: String -> [(Int, Char)]
encode []			= []
encode x			=
	let	c		= head x
		count 	= length (takeWhile (c ==) x)
	in	(count, c) : (encode (drop count x))

decode :: [(Int, Char)] -> String
decode []			= []
decode (x:xs)		=
	replicate (fst x) (snd x) ++ decode xs

longestSubsequence :: (a -> Bool) -> [a] -> Int
longestSubsequence p [] = 0
longestSubsequence p x
	| rest > self	= rest
	| otherwise		= self
	where
		self		= length (takeWhile p x)
		rest		= longestSubsequence p (drop (self + 1) x)
{-longestSubsequence p x =
	let
		self		= length (takeWhile p x)
		rest		= longestSubsequence p (drop (self + 1) x)
	in case () of
		_ | rest > self		= rest
		_ | otherwise		= self-}

data Tree a 		= Nil | Fork a (Tree a) (Tree a)
	deriving Show

tmap :: (a -> b) -> Tree a -> Tree b
tmap p Nil			= Nil
tmap p (Fork x a b)	= Fork (p x) (tmap p a) (tmap p b)

cutOff :: Int -> Tree a -> Tree a
cutOff 0 _				= Nil
cutOff _ Nil			= Nil
cutOff len (Fork x a b)	= Fork x (cutOff (len - 1) a) (cutOff (len - 1) b)
