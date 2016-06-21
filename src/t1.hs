
digits 0 = [] 
digits n = n `mod` 10 : (digits $ n `div` 10)
sumDigits = sum . digits 

trips = filter (\x -> (sumDigits x) == (sumDigits $ x*3))

main = do
   print $ map (\x -> x `div` 9) $ trips [10..99]
