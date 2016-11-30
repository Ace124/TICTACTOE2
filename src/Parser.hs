 module Parser
 where
 import Data.Char
 import Data.List
--to filter items with odd index
 odds :: [t] -> [t]
 odds [] = []
 odds [x] = []
 odds (e1:e2:xs) = e2 : odds xs
 
 parseJson :: [Char] -> [(Char, Char, Char)]
 parseJson j = 
  let
   list = removeGarbage j
   newList = cnv list
  in newList
 
 --to get rid of everything unnecessary
 removeGarbage :: [Char] -> [Char]
 removeGarbage g = (odds . removeSeaparator . removeWhiteSpace . removeLeftBrace . removeRightBrace . removeQuote) g
 
 removeWhiteSpace :: [Char] -> [Char]
 removeWhiteSpace t = deleteAllInstances ' ' t
 
 removeLeftBrace :: [Char] -> [Char]
 removeLeftBrace l = deleteAllInstances '[' l
 
 removeRightBrace :: [Char] -> [Char]
 removeRightBrace r = deleteAllInstances ']' r
 
 removeSeaparator :: [Char] -> [Char]
 removeSeaparator s = deleteAllInstances ',' s
 
 removeQuote :: [Char] -> [Char]
 removeQuote r = deleteAllInstances '"' r
 
 countX :: String -> Int
 countX str = count str 'x'
 
 countO :: String -> Int
 countO str = count str 'o'
 
 --Recursively delete unwanted characters
 deleteAllInstances :: Eq a => a -> [a] -> [a]
 deleteAllInstances a (x:xs)
    | a == x    = rest
    | otherwise = x : rest
      where
        rest = deleteAllInstances a xs
 deleteAllInstances _ _ = [] 
 
 -- Count letters in string
 count :: Eq a => [a] -> a -> Int
 count str c = length $ filter (== c) str
 
  --Convert string into list of tuples
 cnv :: [t] -> [(t, t, t)]
 cnv [] = []
 cnv (l:v:k:t) = (l, v, k) : cnv t