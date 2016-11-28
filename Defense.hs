 module Defense
 where

 type Move = (Int, Int, Char)

 move :: String -> Maybe Move
 move m =
  let
   list = removeGarbage m
   xCount = countX list
   oCount = countO list
   newList = cnv list
   value = if xCount <= oCount then 'x' else 'o'
   answer = case findEmptySquare newList of
    Nothing -> Nothing
    Just (x,y) -> if null newList then Just (0, 0, value) else Just (x, y, value)
  in answer
  
 --Find empty square if possible 
 findEmptySquare :: (Num t2, Num t3, Foldable t) => t (Char, Char, t1) -> Maybe (t2, t3)
 findEmptySquare list =
  let
    squares = [('0','0'),('0','1'),('0','2'),('1','0'),('1','1'),('1','2'),('2','0'),('2','1'),('2','2')]
    emptySquares = filter (\s -> all (not . isSameSquare s) list) squares
    emptySquare = if null emptySquares then Nothing else Just $ readC $ head emptySquares
  in emptySquare
 
 --Check if squares from list match with available squares
 isSameSquare :: (Eq a, Eq a1) => (a, a1) -> (a, a1, t) -> Bool
 isSameSquare (x1, y1) (x2, y2, _) = (x1 == x2) && (y1 == y2)

 --Convert string into list of tuples
 cnv :: [t] -> [(t, t, t)]
 cnv [] = []
 cnv (l:v:k:t) = (l, v, k) : cnv t
 
 readC :: (Num t, Num t1) => (Char, Char) -> (t, t1)
 readC ('0','0') = (0, 0)
 readC ('0','1') = (0, 1) 
 readC ('0','2') = (0, 2)
 readC ('1','0') = (1, 0)
 readC ('1','1') = (1, 1) 
 readC ('1','2') = (1, 2)
 readC ('2','0') = (2, 0)
 readC ('2','1') = (2, 1) 
 readC ('2','2') = (2, 2)
 readC _ = error "Coordinates expected"
 
 --to filter items with odd index
 odds :: [t] -> [t]
 odds [] = []
 odds [x] = []
 odds (e1:e2:xs) = e2 : odds xs
 
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