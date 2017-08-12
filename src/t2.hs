
splitsemi = split ';'

split :: Char -> String -> [String]
split _ [] = [[]]
split ch str = let (h,t) = break (==ch) str 
                in case t of [] -> [h]
                             (_:t1) -> h : split ch t1

main :: IO ()
main = print $ split ';' "123;456;789;abc;def" 
