{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Defense
import Validate
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types.Header
import Data.Char
import Data.Tuple.Select
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Lazy.Char8 as C
import Data.Maybe

--url :: String
url = "http://tictactoe.homedir.eu/game/n11/player/2"

post :: String -> IO()
post msg= do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow url
  let request = initialRequest { method = B.pack "POST",
   requestHeaders = [(hContentType,B.pack "application/json+list"),(hAccept,B.pack "application/json+list")],
   requestBody = RequestBodyLBS $ C.pack ((Prelude.take ((Prelude.length msg) - 1) msg)++", "++(fromJust (turn msg)))
   }
  response <- httpLbs request manager
  Prelude.putStrLn $ msg
  Prelude.putStrLn $ "The status code was atejo: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  if(validate ((Prelude.take ((Prelude.length msg) - 1) msg)++", "++(fromJust (turn msg))) == False) 
    then get
    else Prelude.putStrLn $ "Game over "
get :: IO()
get = do
  manager <- newManager defaultManagerSettings
  initialRequest <- parseUrlThrow url
  let request = initialRequest { method = "GET",
   requestHeaders = [(hContentType, "application/json+list"),(hAccept, "application/json+list")]
   }
  response <- httpLbs request manager
  Prelude.putStrLn $ "The status code was1: " ++ (show $ statusCode $ responseStatus response)
  post $ unpack $ responseBody response
main :: IO()
main = do
    get