import HW4

import System.Environment

-- run readItem on each line of the file and concatenate the results
readAllItems :: String -> String
readAllItems inp = 
    let
        process :: String -> String
        process line = show (readItem line)
    in
        unlines (map process (lines inp))

-- entry point for the program
main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> do
            input <- getContents
            putStr (readAllItems input)
        [infile] -> do
            input <- readFile infile
            putStr (readAllItems input)
        [infile,outfile] -> do
            input <- readFile infile
            writeFile outfile (readAllItems input)
        _ -> putStrLn "usage: ./hw4 [infile [outfile]]"
