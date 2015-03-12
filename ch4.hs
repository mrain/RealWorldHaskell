import System.Environment

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