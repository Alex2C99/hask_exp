
split :: Char -> String -> [String]
split ch = split' ch []
   where 
      split' :: Char -> String -> String -> [String]
      split'  _ cs [] = [reverse cs]
      split' ch cs (t:ts) 
          | t == ch   = reverse cs : split' ch [] ts
          | otherwise = split' ch (t:cs) ts

splitsemi = split ';'

main :: IO ()
main = print $ splitsemi "123;456;789;abc;def" 
