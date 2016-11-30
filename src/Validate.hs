 module Validate
 where
 import Parser

 validate :: String -> Bool
 validate str = validateMoves (parseJson str)

 validateMoves :: [(Char,Char,Char)] -> Bool
 validateMoves moves
    -- Checks for last turn  
    | (length(moves)) == 9 = True
    -- Checks columns for winner 
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='1' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='2' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='0' && c=='x') moves))==3)) = True
	| (((length(filter(\(a,b,c)-> a=='0' && c=='o') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='1' && c=='o') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='2' && c=='o') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> a=='0' && c=='o') moves))==3)) = True
    -- Checks rows for winner
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='1' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='2' && c=='x') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='0' && c=='x') moves))==3)) = True
	| (((length(filter(\(a,b,c)-> b=='0' && c=='o') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='1' && c=='o') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='2' && c=='o') moves))==3)) = True
    | (((length(filter(\(a,b,c)-> b=='0' && c=='o') moves))==3)) = True
    -- Checks diagonals for winner
    | (((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='x') moves))==1)) = True
    | (((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='x') moves))==1) && ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='x') moves))==1)) = True
	| (((length(filter(\(a,b,c)-> a=='0' && b=='0' && c=='o') moves))==1) && ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves))==1) && ((length(filter(\(a,b,c)-> a=='2' && b=='2' && c=='o') moves))==1)) = True
    | (((length(filter(\(a,b,c)-> a=='2' && b=='0' && c=='o') moves))==1) && ((length(filter(\(a,b,c)-> a=='1' && b=='1' && c=='o') moves))==1) && ((length(filter(\(a,b,c)-> a=='0' && b=='2' && c=='o') moves))==1)) = True
    | otherwise = False