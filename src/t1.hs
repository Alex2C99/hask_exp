import System.IO

digits 0 = [] 
digits n = n `mod` 10 : digits (n `div` 10)
sumDigits = sum . digits 

trips = filter (\x -> sumDigits x == sumDigits (x*3))

getInt = read <$> getLine

prompt :: String -> IO ()
prompt pmt = do
   putStr $ pmt ++ ": "
   hFlush stdout

-- main = print $ map (\x -> x `div` 9) $ trips [10..99]

main = do
   prompt "Enter integer"
   a <- getInt
   print $ sumDigits a
