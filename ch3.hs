-- Exercise 1
myLength [] = 0
myLength (x:xs) = myLength xs + 1

-- Exercise 4
palindrome xs = xs ++ (reverse xs)

-- Exercise 5
testPalindrome [] = True
testPalindrome (x:[]) = True
testPalindrome xs = if (head xs) == (last xs) then (testPalindrome $ take (length xs - 2) (tail xs)) else False

-- Exercise 6
import Data.List
mySort xs = sortBy (\x->(\y->compare (length x) (length y))) xs

-- Exercise 7
myIntersperse :: a -> [[a]] -> [a]
myIntersperse p [] = []
myIntersperse p (x:[]) = x
myIntersperse p (x:xs) = x ++ (p:(myIntersperse p xs))

-- Exercise 8
data Tree a = Node a (Tree a) (Tree a)
			| Empty
			deriving(Show)

height :: Tree a -> Int
height Empty = 0
height (Node a left right) = (max (height left) (height right)) + 1

-- Exercise 9,10
data Point = Point {
	x :: Double,
	y :: Double
} deriving (Show)

data Direction = TurnLeft
			   | TurnRight
			   | Straight
			   deriving (Show)


getDirection :: Point -> Point -> Point -> Direction
getDirection (Point x1 y1) (Point x2 y2) (Point x3 y3)
	| prod == 0		= Straight
	| prod < 0		= TurnLeft
	| prod > 0		= TurnRight
	where prod = (x1 - x2) * (y3 - y2) - (x3 - x2) * (y1 - y2)


-- Exercise 11
getDirections :: [Point] -> [Direction]
getDirections xs
	| length xs < 3		= []
	| otherwise			= (getDirection (head xs) (head.tail $ xs) (head.tail.tail $ xs)):(getDirections $ tail xs)