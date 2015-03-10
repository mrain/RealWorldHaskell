import Data.List

data Point = Point Double Double
			 deriving (Show)

crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1
getDirection (Point x1 y1) (Point x2 y2) = Point (x1 - x2) (y1 - y2)
ccw z x y = crossProduct (getDirection x z) (getDirection y z)

prepareList xs = p:(sortBy (\x->(\y->compare 0 (ccw p x y))) $ tail l)
    where l = sortBy (\(Point x1 y1)->(\(Point x2 y2) -> compare y1 y2)) xs
          p = head l

insPoint :: [Point] -> Point -> [Point]
insPoint xs p 
	| length xs == 1							= xs ++ [p]
	| (ccw (last (init xs)) (last xs) p) <= 0		= insPoint (init xs) p
	| otherwise									= xs ++ [p]

scan xs = if (length xs < 3) then l else foldl insPoint (take 2 l) (drop 2 l)
	where l = prepareList xs