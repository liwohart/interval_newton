module Parsing (getOptions, ShowOpt(..)) where

import System.Console.GetOpt
import System.Environment (getArgs)
import Data.Maybe (fromMaybe, maybe)
import Data.Function ((&))
import Data.Char (isDigit)
import InterComp
import Fs

data ShowOpt
 = S
 | P Int
 | R
 | I
 deriving (Show, Read)

data Options = Options
 { function_id :: Int
 , precision  :: Double
 , initial_x  :: Maybe (Double, Double)
 , show_stuff :: ShowOpt
 } deriving Show

type OptionsTuple = (Int, Double, Maybe (Double, Double), ShowOpt)

defaultOpts = Options
 { function_id = 0
 , precision = 0.0
 , initial_x = Nothing
 , show_stuff = S
 }


toTuple :: Options -> OptionsTuple
toTuple (Options f p i s) = (f,p,i,s)

parseShowOpt :: String -> ShowOpt
parseShowOpt s@(c:cs)
 | isDigit c = P $ read s
 | otherwise = read s

options :: [OptDescr (Options -> Options)]
options = 
 [ Option ['f'] ["function-id"]
    (ReqArg ((\n opts -> opts {function_id = n}) . read) "id")
    "function identification number"
 , Option ['p'] ["precision"]
    (ReqArg ((\p opts -> opts {precision = p}) . read) "presicion")
    "precision tolerance"
 , Option ['i'] ["initial-x"]
    (ReqArg ((\i opts -> opts {initial_x = Just i}) . read) "interval")
    "precision tolerance"
 , Option ['s'] ["show-stuff"]
    (OptArg ((\s opts -> opts {show_stuff = s}) . maybe (P 10) parseShowOpt) "show option [S, P [], R, I]")
    "show the calculations"
 ]

parse :: [String] -> IO OptionsTuple
parse argv = case getOpt Permute options argv of
 (o,_,[]) -> return $ toTuple $ foldl (&) defaultOpts o
 (_,_,errors) -> ioError $ userError $ concat errors

getOptions :: IO OptionsTuple
getOptions = getArgs >>= parse
