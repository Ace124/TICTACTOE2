module Gintis
where
import Data.Char
import Data.List
import Network.HTTP.Client
-- import qualified System.IO.Streams as Streams
-- import qualified Data.ByteString as S
 

move :: String -> Maybe (Int, Int, Char)
move str = calculteMove (parse str)

calculteMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
calculteMove moves = findMove moves

parseMove :: Maybe (Int, Int, Char) -> Maybe String
parseMove Nothing = Nothing
parseMove (Just (x,y,z)) = Just ("m[\"x\"; " ++ [(intToDigit x)] ++ "; \"y\"; "++ [(intToDigit y)] ++  "; \"v\"; \"" ++ [z] ++"\"]]")		

turn :: String -> Maybe String
turn msg = parseMove $ move msg 

findMove :: [(Char,Char,Char)] -> Maybe(Int, Int, Char)
findMove moves  
	| (filter(\(a,b,c)-> a=='1' && b=='1') moves)==[] = Just (digitToInt '1',digitToInt '1',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='0' && b=='0') moves)==[] = Just (digitToInt '0',digitToInt '0',whoseTurn moves) 
	| (filter(\(a,b,c)-> a=='0' && b=='2') moves)==[] = Just (digitToInt '0',digitToInt '2',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='2' && b=='0') moves)==[] = Just (digitToInt '2',digitToInt '0',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='1' && b=='0') moves)==[] = Just (digitToInt '1',digitToInt '0',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='1' && b=='2') moves)==[] = Just (digitToInt '1',digitToInt '2',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='2' && b=='1') moves)==[] = Just (digitToInt '2',digitToInt '1',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='0' && b=='1') moves)==[] = Just (digitToInt '0',digitToInt '1',whoseTurn moves)
	| (filter(\(a,b,c)-> a=='2' && b=='2') moves)==[] = Just (digitToInt '2',digitToInt '2',whoseTurn moves)
	| otherwise = Nothing

whoseTurn :: [(Char,Char,Char)] -> Char
whoseTurn moves
	|(mod (length moves) 2) == 0 = 'x'
	|(mod (length moves) 2) == 1 = 'o'
parse :: String -> [(Char,Char,Char)]
parse "" = error "no list"
parse ('l' : '[' :rest) = parse' (take ((length rest) -1) rest)
parse _ = error "invalid type"

parse' :: String -> [(Char,Char,Char)]
parse' "" = error "nothing in list"
parse' str = parse'' str []

parse'' :: String -> [(Char,Char,Char)] -> [(Char,Char,Char)]
parse'' str [] = 
    let
		x = take 1 (drop 7 str)
		x' = head x
		y = take 1 (drop 15 str)
		y' = head y
		v = take 1 (drop 24 str)
		v' = head v
		rest = drop 28 str
	in parse'' rest ((x',y',v') : [])
	
parse'' (' ':str) acc =
	let
		x = take 1 (drop 7 str)
		x' = head x
		y = take 1 (drop 15 str)
		y' = head y
		v = take 1 (drop 24 str)
		v' = head v
		rest = drop 28 str
	in parse'' rest ((x',y',v'):acc)
parse'' "" acc = acc