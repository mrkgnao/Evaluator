module Replace (replace, replaceList) where

startsWith :: (Eq a) => [a] -> [a] -> (Bool, [a])
startsWith x [] = (True, x)
startsWith [] _ = (False, [])
startsWith (x:xs) (y:ys)
    | x == y = startsWith xs ys
    | otherwise = (False, [])

replace' :: (Eq a) => [a] -> [a] -> [a] -> [a] -> [a]
replace' [] prefix _ _ = prefix
replace' list _ [] _ = list
replace' (x:list) prefix oldSubList newSubList
    | fst pair = ( prefix ++ newSubList ++ suffix)
    | otherwise = replace' list (prefix ++ [x]) oldSubList newSubList
  where
    pair = startsWith (x:list) oldSubList
    suffix = replace' (snd pair) [] oldSubList newSubList

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace list oldSubList newSubList = replace' list [] oldSubList newSubList

-- takes a list of replacements to be made an applies them in order.
replaceList :: (Eq a) => [a] -> [ ( [a],[a] ) ] -> [a]
replaceList list [] = list
replaceList list (x:xs) = replaceList (replace list (fst x) (snd x)) xs