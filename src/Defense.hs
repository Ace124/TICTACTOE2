 module Defense
 where
 import Parser
 import Data.Char
 import Data.List
 import Network.HTTP.Client
 
 move :: String -> Maybe (Int, Int, Char)
 move str = calculteMove (parseJson str)

 calculteMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
 calculteMove moves = if (tryToWin moves == Nothing)
                         then findMove moves
                         else tryToWin moves
                         
 encodeMove :: Maybe (Int, Int, Char) -> Maybe String
 encodeMove Nothing = Nothing
 encodeMove (Just (x,y,z)) = Just ("[\"x\", " ++ [(intToDigit x)] ++ ", \"y\", "++ [(intToDigit y)] ++  ", \"v\", \"" ++ [z] ++"\"]]")
 
 turn :: String -> Maybe String
 turn msg = encodeMove $ move msg 

 findMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
 findMove moves
    | (((length(moves))==1) && ((filter(\(a,b,c)-> a=='0' && b=='0') moves)==[])) = Just (digitToInt '0',digitToInt '0','o')
    | (((length(moves))==1) && ((filter(\(a,b,c)-> a=='2' && b=='0') moves)==[])) = Just (digitToInt '2',digitToInt '0','o')
    -- If x = 0
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='0') moves)==[])) = Just (digitToInt '0',digitToInt '0','o')
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='1') moves)==[])) = Just (digitToInt '0',digitToInt '1','o')
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='2') moves)==[])) = Just (digitToInt '0',digitToInt '2','o')
    -- If x = 2
    | (((length(filter(\(a,b,c)-> a=='2' && c=='x') moves))==2) && ((filter(\(a,b,c)-> a=='2' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='0') moves)==[])) = Just (digitToInt '2',digitToInt '0','o')
    | (((length(filter(\(a,b,c)-> a=='2' && c=='x') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='1') moves)==[])) = Just (digitToInt '2',digitToInt '1','o')
    | (((length(filter(\(a,b,c)-> a=='2' && c=='x') moves))==2) && ((filter(\(a,b,c)-> a=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='2') moves)==[])) = Just (digitToInt '2',digitToInt '2','o')
    -- If y = 0
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==2) && ((filter(\(a,b,c)-> b=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='0') moves)==[])) = Just (digitToInt '0',digitToInt '0','o')
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==2) && ((filter(\(a,b,c)-> b=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='1' && b=='0') moves)==[])) = Just (digitToInt '1',digitToInt '0','o')
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==2) && ((filter(\(a,b,c)-> b=='0' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='0') moves)==[])) = Just (digitToInt '2',digitToInt '0','o')
    -- If y = 2
    | (((length(filter(\(a,b,c)-> b=='2' && c=='x') moves))==2) && ((filter(\(a,b,c)-> b=='2' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='0' && b=='2') moves)==[])) = Just (digitToInt '0',digitToInt '2','o')
    | (((length(filter(\(a,b,c)-> b=='2' && c=='x') moves))==2) && ((filter(\(a,b,c)-> b=='2' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='1' && b=='2') moves)==[])) = Just (digitToInt '1',digitToInt '2','o')
    | (((length(filter(\(a,b,c)-> b=='2' && c=='x') moves))==2) && ((filter(\(a,b,c)-> b=='2' && c=='o') moves)==[]) && ((filter(\(a,b,c)-> a=='2' && b=='2') moves)==[])) = Just (digitToInt '2',digitToInt '2','o')
    -- Diagonals defense
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[]) = Just (digitToInt '2',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[]) = Just (digitToInt '0',digitToInt '0','o')
    
    | ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[]) = Just (digitToInt '2',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[]) = Just (digitToInt '0',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    -- Cross defense
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[]) = Just (digitToInt '2',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[]) = Just (digitToInt '0',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    
    | ((length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[]) = Just (digitToInt '1',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[]) = Just (digitToInt '1',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='x') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='x') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    -- If its impossible to loose
    | (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[] = Just (digitToInt '0',digitToInt '0','o')
    | (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[] = Just (digitToInt '2',digitToInt '0','o')
    | (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[] = Just (digitToInt '0',digitToInt '2','o')
    | (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[] = Just (digitToInt '2',digitToInt '2','o')
    | (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[] = Just (digitToInt '0',digitToInt '1','o')
    | (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[] = Just (digitToInt '1',digitToInt '0','o')
    | (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[] = Just (digitToInt '1',digitToInt '2','o')
    | (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[] = Just (digitToInt '2',digitToInt '1','o')
    | otherwise = Nothing
 
 tryToWin :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
 tryToWin moves 
    -- Diagonals offensive
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[]) = Just (digitToInt '2',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[]) = Just (digitToInt '0',digitToInt '0','o')
    
    | ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[]) = Just (digitToInt '2',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[]) = Just (digitToInt '0',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    -- Cross offensive
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[]) = Just (digitToInt '2',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[]) = Just (digitToInt '0',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    
    | ((length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[]) = Just (digitToInt '1',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[]) = Just (digitToInt '1',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    -- Columns offensive
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[]) = Just (digitToInt '2',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[]) = Just (digitToInt '0',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[]) = Just (digitToInt '1',digitToInt '0','o')
    
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[]) = Just (digitToInt '2',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[]) = Just (digitToInt '0',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    
    | ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[]) = Just (digitToInt '1',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[]) = Just (digitToInt '2',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[]) = Just (digitToInt '0',digitToInt '2','o')
    -- Rows offensive
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[]) = Just (digitToInt '0',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='1' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[]) = Just (digitToInt '0',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[]) = Just (digitToInt '0',digitToInt '1','o')
    
    | ((length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[]) = Just (digitToInt '1',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[]) = Just (digitToInt '1',digitToInt '0','o')
    | ((length(filter(\(a,b,c)-> a=='1' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='1' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[]) = Just (digitToInt '1',digitToInt '1','o')
    
    | ((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[]) = Just (digitToInt '2',digitToInt '2','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[]) = Just (digitToInt '2',digitToInt '1','o')
    | ((length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves)==1) && (length(filter(\(a,b,c)-> a=='2' && b=='1' && c=='o') moves)==1) && (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[]) = Just (digitToInt '2',digitToInt '0','o')
    | otherwise = Nothing