import System.Environment
import Data.Char(ord)

-- Exercise 1
safeHead xs = if null xs then Nothing else Just $ head xs
safeTail xs = if null xs then Nothing else Just $ tail xs
safeLast xs = if null xs then Nothing else Just $ last xs
safeInit xs = if null xs then Nothing else Just $ init xs

-- Exercise 2
splitWith f xs = let (pre, suf) = break f xs 
                 in if (null suf) then [pre] else pre:(splitWith f (tail suf))

-- Exercise 3
interactWith f inputFile outputFile = do
        input <- readFile inputFile
        writeFile outputFile (f input)

main = mainWith myFunction
    where mainWith myFunction = do
            args <- getArgs
            case args of 
                [inputFile, outputFile]    -> interactWith myFunction inputFile outputFile
                _                        -> putStrLn "error: exactly two arguments needed"

          myFunction = unlines . (map (head . words)) . lines

-- Exercise 4
myTranspose content = unlines . recur $ lines content
    where emptyHead str = if null str then ' ' else head str
          emptyTail str = if null str then "" else tail str
          recur xs = if all null xs then []
                           else (map emptyHead xs):(recur $ (filter (not . null)) (map emptyTail xs))

asInt_fold s = if (head s == '-') then -(copWith $ tail s) else copWith s
    where toDigit c = let a = (ord c) - (ord '0') in if (elem a [0..9]) then a else error ("not a dight '" ++ [c] ++ "'")
          copWith s = foldl (\x->(\y->x * 10 + (toDigit y))) 0 s

concat_foldl xs = foldl (++) [] xs

concat_foldr xs = foldr (flip (foldr (:))) [] xs

takeWhile_mine _ [] = []
takeWhile_mine f (x:xs) = if (f x) then x:(takeWhile_mine f xs) else []

takeWhile_foldr f xs = foldr comb [] xs
    where comb x ys = if (f x) then x:ys else []

groupBy_fold f xs = foldr comb [] xs
    where comb x [] = [[x]]
          comb x (y:ys) = if (f x (head y)) then (x:y):ys else [x]:y:ys

any_fold f xs = foldl (flip ((||) . f)) False xs
cycle_fold xs = foldl (flip (\x->(++) xs)) [] [1..]